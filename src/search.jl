using Plots

mutable struct Program{D}
    expr::SExpr{D}
    id::Int
    task::Int
end

mutable struct Corpus{D}
    programs::Vector{Program{D}}
    programs_by_task::Dict{Int, Vector{Program{D}}}

    function Corpus(programs)
        tasks = unique([p.task for p in programs])
        programs_by_task = Dict(t => [p for p in programs if p.task == t] for t in tasks)
        new{Match}(programs, programs_by_task)
    end
end

Base.show(io::IO, obj::Corpus) = print(io, "\n\t", join(obj.programs, "\n\t"))
Base.show(io::IO, obj::Program) = print(io, obj.expr)

size(p::Program) = size(p.expr)

function size(corpus::Corpus) :: Float32
    sum(minimum.(size, values(corpus.programs_by_task)))
end

mutable struct Match
    expr::SExpr{Match} # pointer to subtree in original corpus
    all_args::Vector{SExpr{Match}}
    unique_args::Vector{SExpr{Match}} # pointers to first instance of each arg within subtree ie args[1] is #0
    holes::Vector{SExpr{Match}} # pointers to holes within subtree
    holes_stack::Vector{SExpr{Match}} # expanded holes
    local_utility_stack::Vector{Float32} # past utilities
    program::Program{Match} # which program this subtree appears in
    size::Float32
    num_nodes::Int
    struct_hash::Int

    # Tracks Eqn 12: https://arxiv.org/pdf/2211.16605.pdf
    local_utility::Float32
    
    cumulative_utility::Float32
    accept_rewrite::Bool
    is_active::Bool
    id::Int

    Match(expr, program, id) = new(expr, [], [], [expr], [], [], program, size(expr), num_nodes(expr), struct_hash(expr), local_utility_init(), NaN32, false, false, id)
end

abstract type Expansion end

struct PossibleExpansion{T <: Expansion}
    matches::Vector{Match}
    data::T

    function PossibleExpansion(matches, data::T) where {T}
        new{T}(matches, data)
    end
end

struct SyntacticExpansion <: Expansion
    head::Symbol
    num_holes::Int
end

Base.show(io::IO, obj::SyntacticExpansion) = pretty_show(io, obj; indent=false)

struct AbstractionExpansion <: Expansion
    index::Int
    fresh::Bool
end

Base.show(io::IO, obj::AbstractionExpansion) = pretty_show(io, obj; indent=false)

mutable struct Abstraction
    body::SExpr
    arity::Int
end

Base.show(io::IO, obj::Abstraction) = pretty_show(io, obj; indent=false)




Base.copy(abstraction::Abstraction) = Abstraction(copy(abstraction.body), abstraction.arity)

mutable struct Stats
    expansions::Int
    completed::Int

    comparable_worklist_steps::Int
end

Base.show(io::IO, obj::Stats) = pretty_show(io, obj; indent=true)




mutable struct SearchState
    abstraction::Abstraction
    corpus::Corpus{Match}
    stats::Stats
    new_abstraction_name::Symbol
    track::Union{String, Nothing}

    holes::Vector{SExpr{Match}}
    matches::Vector{Match}
    all_nodes::Vector{SExpr{Match}} # all treenodes in bottom up order - like a version of .matches that is never filtered down
    expansions::Vector{PossibleExpansion}

    holes_stack::Vector{SExpr{Match}}
    expansions_stack::Vector{Vector{PossibleExpansion}}
    matches_stack::Vector{Vector{Match}}
    past_expansions::Vector{PossibleExpansion}

    function SearchState(corpus, new_abstraction_name, track)
        abstraction = Abstraction(new_hole(nothing), 0)
        matches = init_all_corpus_matches(corpus)
        all_nodes = map(m -> m.expr, matches)
        new(abstraction, corpus, Stats(0,0,0), new_abstraction_name, track,
            [abstraction.body], matches, all_nodes, PossibleExpansion[],
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
            expr.data = match
            push!(matches, match)
            id += 1
        end
    end
    matches
end

function is_tracked(search_state; expansion=nothing)
    !isnothing(search_state.track) || return false

    isnothing(expansion) || expand_general!(search_state, expansion)

    body = string(search_state.abstraction.body)
    suffix = split(body, "??")[end]

    isnothing(expansion) || unexpand_general!(search_state)

    endswith(search_state.track, suffix)
end

function is_tracked_pruned(search_state; expansion=nothing, message="message here")
    if is_tracked(search_state, expansion=expansion)
        isnothing(expansion) || expand_general!(search_state, expansion)
        printstyled("TRACK (PRUNED): ", search_state, "\n", color=:red, bold=true)
        isnothing(expansion) || unexpand_general!(search_state)
        printstyled("Reason: ", message, "\n", color=:red, bold=true)
    end
end

Base.@kwdef mutable struct PlotData
    best_util::Vector{Tuple{Int,Float32}} = [(0,0.)]
    depth::Vector{Tuple{Int,Int}} = [(0,0)]
    num_matches::Vector{Tuple{Int,Int}} = []
    upper_bound::Vector{Tuple{Int,Float32}} = []
    size_matches::Vector{Tuple{Int,Float32}} = []
    completed_util::Vector{Tuple{Int,Float32}} = [(0,0.)]
    completed_approx_util::Vector{Tuple{Int,Float32}} = [(0,0.)]
    pruned_bound::Vector{Tuple{Int,Float32}} = [(0,0.)]
end

function stitch_search(corpus, new_abstraction_name; max_arity=2, upper_bound_fn=upper_bound_with_conflicts, expansion_processor=nothing, verbose=false, verbose_best=true, track=nothing, follow=false, plot=false, silent=false, allow_single_task=true)

    best_util = Float32(0)
    best_abstraction = nothing
    search_state = SearchState(corpus, new_abstraction_name, track)

    plot_data = PlotData()

    needs_expansion = true

    while true
        # At each step of this loop we are either about to start exploring a new level of
        # depth in the search tree (needs_expansion=true) or we are continuing search at an
        # existing level (needs_expansion=false)
        if needs_expansion
            possible_expansions!(search_state, max_arity, upper_bound_fn, best_util)
            if !isnothing(expansion_processor)
                process_expansions!(search_state, expansion_processor)
            end


            !verbose || printstyled(search_state, "\n", color=:yellow);
            # !verbose || println("possible_expansions!() -> ", length(search_state.expansions), " ", [e.data for e in search_state.expansions])
            needs_expansion = false
            search_state.stats.comparable_worklist_steps += 1
            plot && push!(plot_data.depth, (search_state.stats.expansions, length(search_state.past_expansions)))
            plot && push!(plot_data.num_matches, (search_state.stats.expansions, length(search_state.matches)))
            continue
        end

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
        if upper_bound_fn(search_state,expansion) <= best_util
            is_tracked_pruned(search_state, expansion=expansion, message="$(@__FILE__):$(@__LINE__) - upper bound $(upper_bound_fn(search_state,expansion)) <= best util $best_util")
            plot && push!(plot_data.pruned_bound, (search_state.stats.expansions, upper_bound_fn(search_state,expansion)))
            continue # skip - worse than best so far
        end
        
        # do the expansion
        expand_general!(search_state, expansion)

        # for when we are tracking a specific abstraction
        if is_tracked(search_state)
            silent || printstyled("TRACK: ", search_state.abstraction.body, "\n", color=:green, bold=true)
        elseif follow && !is_tracked(search_state)
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
        if !allow_single_task && is_single_task(search_state)
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

            if approx_util <= best_util
                continue # skip - worse than best so far
            end

            # eval util
            util = bottom_up_utility(search_state)

            # upper_bound_fn(search_state) >= util || error("upper bound is not valid")

            plot && push!(plot_data.completed_util, (search_state.stats.expansions, util))

            # check for new best
            if util > best_util
                best_util = util
                best_abstraction = copy(search_state.abstraction)
                !verbose_best || printstyled("[step=$(search_state.stats.expansions)] new best: ", search_state.abstraction.body, " with utility ", best_util, " used in $(length(search_state.matches)) places\n", color=:green)
                plot && push!(plot_data.best_util, (search_state.stats.expansions, best_util))
            end

            # return now if this is `follow=true`
            if follow
                string(search_state.abstraction.body) == track || error("shouldnt be possible")
                plot && break
                return search_state
            end

            continue
        end

        needs_expansion = true
    end

    if isnothing(best_abstraction)
        silent || println("No abstractions found")
    else 
        silent || println("Best abstraction: ", best_abstraction.body, " with utility ", best_util, " compressed by ", size(search_state.corpus) / (size(search_state.corpus) - best_util), "x");
    end

    silent || println(search_state.stats);

    # plot
    if plot
        # normalize utilities
        plot_data.best_util = [(x, y/best_util) for (x,y) in plot_data.best_util]
        plot_data.completed_util = [(x, y/best_util) for (x,y) in plot_data.completed_util]
        plot_data.completed_approx_util = [(x, y/best_util) for (x,y) in plot_data.completed_approx_util]
        plot_data.pruned_bound = [(x, y/best_util) for (x,y) in plot_data.pruned_bound]
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
    


    isnothing(best_abstraction) && return nothing

    !follow || plot || error("shouldnt be possibleee")

    # recurse, but with follow=true so that we rapidly narrow in on the best abstraction
    # then the search state at that point gets returned
    res = stitch_search(corpus, new_abstraction_name; max_arity=10000, upper_bound_fn=upper_bound_fn, verbose=false, verbose_best=false, track=string(best_abstraction.body), follow=true, silent=true, allow_single_task=true, plot=false)
    isnothing(res) && error("shouldnt be possible - we found it the first time around without tracking")
    res
end


mutable struct SearchResult
    rewritten::Corpus
    abstraction::Abstraction
    util::Float32
end

function compress(original_corpus; iterations=3, kwargs...)
    corpus = original_corpus
    for i in 1:iterations
        println("===Iteration $i===")
        search_res = stitch_search(corpus, Symbol("fn_$i"); kwargs...)
        if isnothing(search_res)
            println("No more abstractions")
            break
        end
        (rewritten, compressive, cumulative) = rewrite(search_res)
        corpus = rewritten
    end
    println("Total compression: ", size(original_corpus) / size(corpus), "x")
end


