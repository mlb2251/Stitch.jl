using Plots


mutable struct Corpus
    programs::Vector{Program}
    programs_by_task::Dict{Int, Vector{Program}}

    function Corpus(programs)
        tasks = unique([p.task for p in programs])
        programs_by_task = Dict(t => [p for p in programs if p.task == t] for t in tasks)
        new(programs, programs_by_task)
    end
end

Base.show(io::IO, obj::Corpus) = print(io, "\n\t", join(obj.programs, "\n\t"))
Base.show(io::IO, obj::Program) = print(io, obj.expr)

size(p::Program) = size(p.expr)

function size(corpus::Corpus) :: Float32
    sum(minimum.(size, values(corpus.programs_by_task)))
end



abstract type Expansion end

struct PossibleExpansion{T <: Expansion}
    matches::Vector{Match}
    data::T

    function PossibleExpansion(matches, data::T) where {T}
        new{T}(matches, data)
    end
end

struct SyntacticLeafExpansion <: Expansion
    leaf::Symbol
end

struct SyntacticNodeExpansion <: Expansion
    num_holes::Int
end

Base.show(io::IO, obj::SyntacticLeafExpansion) = pretty_show(io, obj; indent=false)
Base.show(io::IO, obj::SyntacticNodeExpansion) = pretty_show(io, obj; indent=false)

struct AbstractionExpansion <: Expansion
    index::Int
    fresh::Bool
end

Base.show(io::IO, obj::AbstractionExpansion) = pretty_show(io, obj; indent=false)

struct SymbolExpansion <: Expansion
    idx::Int
end

Base.show(io::IO, obj::SymbolExpansion) = pretty_show(io, obj; indent=false)


mutable struct Abstraction
    body::SExpr
    arity::Int
end

Base.show(io::IO, obj::Abstraction) = pretty_show(io, obj; indent=false)

Base.copy(abstraction::Abstraction) = Abstraction(copy(abstraction.body), abstraction.arity)

@Base.kwdef mutable struct Stats
    expansions::Int = 0
    completed::Int = 0

    comparable_worklist_steps::Int = 0
end

Base.show(io::IO, obj::Stats) = pretty_show(io, obj; indent=true)


Base.@kwdef mutable struct SearchConfig
    new_abstraction_name::Symbol = :placeholder
    track::Union{SExpr, Nothing} = nothing
    max_arity::Int = 2
    upper_bound_fn::Function = upper_bound_with_conflicts
    expansion_processor::Union{Function, Nothing} = nothing
    verbose::Bool = false
    verbose_best::Bool = true
    follow::Bool = false
    plot::Bool = false
    silent::Bool = false
    allow_single_task::Bool = true
    no_opt_arg_capture::Bool = false
    no_opt_redundant_args::Bool = false
end


Base.@kwdef mutable struct PlotData
    normalized::Bool = false
    best_util::Vector{Tuple{Int,Float32}} = [(0,0.)]
    depth::Vector{Tuple{Int,Int}} = [(0,0)]
    num_matches::Vector{Tuple{Int,Int}} = []
    upper_bound::Vector{Tuple{Int,Float32}} = []
    size_matches::Vector{Tuple{Int,Float32}} = []
    completed_util::Vector{Tuple{Int,Float32}} = [(0,0.)]
    completed_approx_util::Vector{Tuple{Int,Float32}} = [(0,0.)]
    pruned_bound::Vector{Tuple{Int,Float32}} = [(0,0.)]
end

mutable struct SearchState
    # config
    config::SearchConfig
    corpus::Corpus
    all_nodes::Vector{SExpr} # all treenodes in bottom up order - like a version of .matches that is never filtered down

    # running data
    plot_data::PlotData
    best_util::Float32
    best_abstraction::Union{Nothing, Abstraction}
    stats::Stats

    # current abstraction
    abstraction::Abstraction
    holes::Vector{SExpr}
    matches::Vector{Match}
    expansions::Vector{PossibleExpansion}

    # backtracking data
    holes_stack::Vector{SExpr}
    expansions_stack::Vector{Vector{PossibleExpansion}}
    matches_stack::Vector{Vector{Match}}
    past_expansions::Vector{PossibleExpansion}

    function SearchState(corpus, config)
        abstraction = Abstraction(new_hole(nothing), 0)
        matches = init_all_corpus_matches(corpus)
        all_nodes = map(m -> m.expr, matches)
        best_util = Float32(0)
        best_abstraction = nothing
        new(config, corpus, all_nodes,
            PlotData(), best_util, best_abstraction, Stats(),
            abstraction, [abstraction.body], matches, PossibleExpansion[],
            SExpr[], PossibleExpansion[], Match[], PossibleExpansion[])
    end
end

Base.broadcastable(s::SearchState) = Ref(s)

function Base.show(io::IO, search_state::SearchState)
    print(io,
          "abstraction: ", search_state.abstraction.body,
          " | matches: ", length(search_state.matches),
          " | expansions: ", length(search_state.expansions),
    );
end

function normalize!(plot_data::PlotData, search_state::SearchState)
    plot_data.normalized && return
    plot_data.normalized = true
    plot_data.best_util = [(x, y/search_state.best_util) for (x,y) in plot_data.best_util]
    plot_data.completed_util = [(x, y/search_state.best_util) for (x,y) in plot_data.completed_util]
    plot_data.completed_approx_util = [(x, y/search_state.best_util) for (x,y) in plot_data.completed_approx_util]
    plot_data.pruned_bound = [(x, y/search_state.best_util) for (x,y) in plot_data.pruned_bound]
end



"""
Initializes a Match at every subtree in the corpus
Note any filtering to the initial match set should NOT be done here because
downstream we need this for SearchState.all_nodes
"""
function init_all_corpus_matches(corpus) :: Vector{Match}
    matches = Match[]
    id = 1
    for program in corpus.programs
        for expr in subexpressions(program.expr) # child-first traversal
            match = Match(expr, program, id)
            expr.match = match
            push!(matches, match)
            id += 1
        end
    end
    matches
end

function is_tracked(search_state; expansion=nothing)
    isnothing(search_state.config.track) && return false

    isnothing(expansion) || expand_general!(search_state, expansion)

    # body = string(search_state.abstraction.body)
    # suffix = split(body, "??")[end]
    res = could_expand_to(search_state.abstraction.body, search_state.config.track)

    isnothing(expansion) || unexpand_general!(search_state)

    # endswith(search_state.config.track, suffix)
    res
end

function is_tracked_pruned(search_state; expansion=nothing, message="message here")
    if is_tracked(search_state, expansion=expansion)
        isnothing(expansion) || expand_general!(search_state, expansion)
        printstyled("TRACK (PRUNED): ", search_state, "\n", color=:red, bold=true)
        isnothing(expansion) || unexpand_general!(search_state)
        printstyled("Reason: ", message, "\n", color=:red, bold=true)
    end
end

"""
Beginning a new level of search - expand current hole in all possible ways
"""
function expand_search_state!(search_state)
    isempty(search_state.expansions) || error("expand_search_state!() should only be called when there are no expansions left to try")

    possible_expansions!(search_state)
    if !isnothing(search_state.config.expansion_processor)
        process_expansions!(search_state)
    end


    !search_state.config.verbose || printstyled(search_state, "\n", color=:yellow);
    # !verbose || println("possible_expansions!() -> ", length(search_state.expansions), " ", [e.match for e in search_state.expansions])
    search_state.stats.comparable_worklist_steps += 1
    search_state.config.plot && push!(plot_data.depth, (search_state.stats.expansions, length(search_state.past_expansions)))
    search_state.config.plot && push!(plot_data.num_matches, (search_state.stats.expansions, length(search_state.matches)))
end

function stitch_search(corpus, config)

    search_state = SearchState(corpus, config)
    
    (; verbose, verbose_best, plot, silent) = config

    expand_search_state!(search_state)

    while true

        # check if there are no expansions to try, and backtrack if so
        if isempty(search_state.expansions)
            if isempty(search_state.past_expansions)
                !verbose || println("no more expansions to try")
                break
            end

            # !verbose || println("unexpanding with: ", search_state.past_expansions[end].data)
            unexpand_general!(search_state)
            continue
        end

        # pop new expansion
        expansion = pop!(search_state.expansions)

        # upper bound check
        if config.upper_bound_fn(search_state,expansion) <= search_state.best_util
            is_tracked_pruned(search_state, expansion=expansion, message="$(@__FILE__):$(@__LINE__) - upper bound $(config.upper_bound_fn(search_state,expansion)) <= best util $(search_state.best_util)")
            plot && push!(plot_data.pruned_bound, (search_state.stats.expansions, config.upper_bound_fn(search_state,expansion)))
            continue # skip - worse than best so far
        end
        
        # do the expansion
        expand_general!(search_state, expansion)

        # for when we are tracking a specific abstraction
        tracked = is_tracked(search_state)
        if tracked
            silent || printstyled("TRACK: ", search_state.abstraction.body, "\n", color=:green, bold=true)
        elseif config.follow && !tracked
            unexpand_general!(search_state)
            continue
        end

        search_state.stats.expansions += 1

        plot && push!(plot_data.upper_bound, (search_state.stats.expansions, upper_bound_fn(search_state)))
        plot && push!(plot_data.size_matches, (search_state.stats.expansions, sum(match -> max(match.local_utility,0.), search_state.matches)))

        # strict dominance check - https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
        if strictly_dominated(search_state)
            is_tracked_pruned(search_state, message="$(@__FILE__):$(@__LINE__) - strictly dominated")
            unexpand_general!(search_state) # force early unexpansion
            continue
        end

        # https://arxiv.org/pdf/2211.16605.pdf "To avoid overfitting, DreamCoder prunes the abstractions that are only useful in programs from a single task."
        if !config.allow_single_task && is_single_task(search_state)
            is_tracked_pruned(search_state, message="$(@__FILE__):$(@__LINE__) - single task")
            unexpand_general!(search_state) # force early unexpansion
            continue
        end

        # are we done?
        if isempty(search_state.holes)            
            search_state.stats.completed += 1
            
            !verbose || println("completed: ", search_state.abstraction.body, " with utility ", bottom_up_utility(search_state), " used in $(length(search_state.matches)) places")
            
            # cheaply upper bounded version of util that uses no conflict resolution
            approx_util = sum(match -> max(match.local_utility,0.), search_state.matches)

            plot && push!(plot_data.completed_approx_util, (search_state.stats.expansions, approx_util))

            if approx_util <= search_state.best_util
                continue # skip - worse than best so far
            end

            # eval util
            util = bottom_up_utility(search_state)

            # upper_bound_fn(search_state) >= util || error("upper bound is not valid")

            plot && push!(plot_data.completed_util, (search_state.stats.expansions, util))

            # check for new best
            if util > search_state.best_util
                search_state.best_util = util
                search_state.best_abstraction = copy(search_state.abstraction)
                !verbose_best || printstyled("[step=$(search_state.stats.expansions)] new best: ", search_state.abstraction.body, " with utility ", search_state.best_util, " used in $(length(search_state.matches)) places\n", color=:green)
                plot && push!(plot_data.best_util, (search_state.stats.expansions, search_state.best_util))
            end

            # return now if this is `follow=true`
            if config.follow
                string(search_state.abstraction.body) == string(config.track) || error("shouldnt be possible")
                plot && break
                return search_state
            end

            continue
        end

        expand_search_state!(search_state)
    end

    if isnothing(search_state.best_abstraction)
        silent || println("No abstractions found")
    else 
        silent || println("Best abstraction: ", search_state.best_abstraction.body, " with utility ", search_state.best_util, " compressed by ", size(search_state.corpus) / (size(search_state.corpus) - search_state.best_util), "x");
    end

    silent || println(search_state.stats);

    # plot
    if plot
        plot(plot_data, search_state)
    end

    isnothing(search_state.best_abstraction) && return nothing

    !config.follow || plot || error("shouldnt be possibleee")

    # recurse, but with follow=true so that we rapidly narrow in on the best abstraction
    # then the search state at that point gets returned
    config = deepcopy(config)
    config.max_arity=10000
    config.verbose = config.verbose_best = config.plot = false
    config.track = search_state.best_abstraction.body
    config.follow = config.silent = config.allow_single_task = true
    res = stitch_search(corpus,config)
    isnothing(res) && error("shouldnt be possible - we found it the first time around without tracking")
    res
end

function plot(plot_data::PlotData, search_state::SearchState)
    # normalize utilities
    normalize!(plot_data, search_state)

    p = Plots.plot(plot_data.best_util, title="Best Utility Over Time", xlabel="Expansions", ylabel="Utility", linetype=:steppre, xlim=(0, search_state.stats.expansions), ylim=(0,1));
    
    Plots.plot!(p, plot_data.completed_approx_util, seriestype=:scatter, alpha=0.5, label="completed approx util")
    Plots.plot!(p, plot_data.completed_util, seriestype=:scatter, alpha=0.5, label="completed util")
    # Plots.plot!(p, plot_data.pruned_bound, seriestype=:scatter, alpha=0.5, label="pruned bound")

    # Plots.plot!(Plots.twinx(), plot_data.depth, seriestype=:line, z_order=:back, color=:orange, alpha=0.3, ylabel="Depth")
    # Plots.plot!(Plots.twinx(), plot_data.num_matches, seriestype=:line, z_order=:back, color=:blue, alpha=0.3, ylabel="Matches", yaxis=:log)
    # Plots.plot!(Plots.twinx(), plot_data.upper_bound, seriestype=:line, z_order=:back, color=:purple, alpha=0.3, ylabel="Upper Bound", yaxis=:log)
    # Plots.plot!(Plots.twinx(), plot_data.size_matches, seriestype=:line, z_order=:back, color=:purple, alpha=0.3, ylabel="Size * Matches")

    display(p)
end


mutable struct SearchResult
    rewritten::Corpus
    abstraction::Abstraction
    util::Float32
end

function compress(original_corpus; iterations=3, kwargs...)
    corpus = original_corpus
    config = SearchConfig(;kwargs...)
    abstractions = String[]
    for i in 1:iterations
        println("===Iteration $i===")
        config.new_abstraction_name = Symbol("fn_$i")
        search_res = stitch_search(corpus, config)
        if isnothing(search_res)
            println("No more abstractions")
            break
        end
        (rewritten, compressive, cumulative) = rewrite(search_res)
        corpus = rewritten
        push!(abstractions, string(search_res.abstraction.body))
    end
    println("Total compression: ", size(original_corpus) / size(corpus), "x")
    return abstractions
end


