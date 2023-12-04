using Plots
using JSON

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

size(p::Program, size_by_symbol) = size(p.expr, size_by_symbol)

function size(corpus::Corpus, size_by_symbol)::Float32
    sum(minimum.(x -> size(x, size_by_symbol), values(corpus.programs_by_task)))
end



abstract type Expansion end

struct PossibleExpansion{T <: Expansion}
    matches::Vector{MatchPossibilities}
    data::T

    function PossibleExpansion(matches, data::T) where {T}
        new{T}(matches, data)
    end
end

struct SyntacticLeafExpansion <: Expansion
    leaf::Symbol
end

struct SyntacticNodeExpansion <: Expansion
    head::Symbol
    num_holes::Int
end

Base.show(io::IO, obj::SyntacticLeafExpansion) = pretty_show(io, obj; indent=false)
Base.show(io::IO, obj::SyntacticNodeExpansion) = pretty_show(io, obj; indent=false)

struct AbstractionExpansion <: Expansion
    index::Int
    fresh::Bool
    dfa_state::Symbol
end

Base.show(io::IO, obj::AbstractionExpansion) = pretty_show(io, obj; indent=false)

struct ContinuationExpansion <: Expansion
end

Base.show(io::IO, obj::ContinuationExpansion) = pretty_show(io, obj; indent=false)


struct SymbolExpansion <: Expansion
    idx::Int
    fresh::Bool
end

Base.show(io::IO, obj::SymbolExpansion) = pretty_show(io, obj; indent=false)


mutable struct Abstraction
    body::SExpr
    arity::Int
    sym_arity::Int
    dfa_root::Symbol
    dfa_metavars::Vector{Symbol}
    dfa_symvars::Vector{Symbol}
end

Base.show(io::IO, obj::Abstraction) = pretty_show(io, obj; indent=false)

Base.copy(abstraction::Abstraction) = Abstraction(
    copy(abstraction.body), abstraction.arity, abstraction.sym_arity,
    abstraction.dfa_root, copy(abstraction.dfa_metavars), copy(abstraction.dfa_symvars))

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

    # only_match_semi::Bool = false
    autoexpand_head::Bool = false # auto expand head of list
    dfa::Union{Dict{Symbol, Dict{Symbol,Vector{Symbol}}}, Nothing} = nothing

    # optimizations
    no_opt_arg_capture::Bool = false
    no_opt_redundant_args::Bool = false
    size_by_symbol::Union{Nothing,Dict{Symbol,Float32}} = nothing

    # utility
    # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (application utility first term; -cost_t(t_A))
    application_utility_fixed::Float32 = -1.0
    application_utility_metavar::Float32 = 0
    application_utility_symvar::Float32 = 0
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
    # hole_dfa_states::Vector{Symbol}
    matches::Vector{MatchPossibilities}
    expansions::Vector{PossibleExpansion}

    # backtracking data
    holes_stack::Vector{SExpr}
    # hole_dfa_states_stack::Vector{Symbol}
    expansions_stack::Vector{Vector{PossibleExpansion}}
    matches_stack::Vector{Vector{MatchPossibilities}}
    past_expansions::Vector{PossibleExpansion}

    function SearchState(corpus, config)
        abstraction = Abstraction(new_hole(nothing), 0, 0, :uninit_state, [], [])
        matches = init_all_corpus_matches(corpus, config)
        if !isnothing(config.dfa)
            for program in corpus.programs
                run_dfa!(program.expr, config.dfa, :M)
            end
        end
        all_nodes = map(expr_of, matches)
        best_util = Float32(0)
        best_abstraction = nothing
        new(config, corpus, all_nodes,
            PlotData(), best_util, best_abstraction, Stats(),
            abstraction, [abstraction.body], matches, PossibleExpansion[],
            SExpr[], PossibleExpansion[], Match[], PossibleExpansion[])
    end
end

function run_dfa!(expr, dfa, state)
    @assert expr.metadata.dfa_state === :uninit_state
    expr.metadata.dfa_state = state
    is_leaf(expr) && return
    head = expr.children[1]
    @assert is_leaf(head)

    # if head === :list
        
    # end

    child_states = dfa[state][head.leaf]
    # if length(child_states) != length(expr.children) - 1 # a state for everything except the head
    #     @show head
    #     @show child_states
    #     @show expr.children
    #     error()
    # end
    for (i,child) in enumerate(expr.children[2:end])
        if i > length(child_states)
            i %= length(child_states)
            i += 1
        end
        run_dfa!(child, dfa, child_states[i])
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
function init_all_corpus_matches(corpus, config::SearchConfig)::Vector{MatchPossibilities}
    matches = MatchPossibilities[]
    id = 1
    for program in corpus.programs
        for expr in subexpressions(program.expr) # child-first traversal (postorder)
            match = fresh_match_possibilities(expr, id, config)
            expr.match = match
            expr.metadata = Metadata(
                program,
                size(expr, config.size_by_symbol),
                num_nodes(expr),
                struct_hash(expr),
                :uninit_state,
                id
            )
            push!(matches, match)
            id += 1
        end
    end
    matches
end

function filter_init_allowed_matches!(search_state)
    # if search_state.config.only_match_semi
    #     filter!(search_state.matches) do m
    #         length(m.expr.children) == 3 && m.expr.children[1].leaf === :semi
    #     end
    # end
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

    size_by_symbol = config.size_by_symbol
    search_state = SearchState(corpus, config)
    
    (; verbose, verbose_best, plot, silent) = config

    filter_init_allowed_matches!(search_state)

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
            approx_util = sum(match -> max(max_local_utility(match),0.), search_state.matches)

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
        silent || println("Best abstraction: ", search_state.best_abstraction.body, " with utility ", search_state.best_util, " compressed by ", size(search_state.corpus, size_by_symbol) / (size(search_state.corpus, size_by_symbol) - search_state.best_util), "x")
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

function root_dfa_state(search_res)
    dfa_states = [expr_of(m).metadata.dfa_state for m in search_res.matches]
    dfa_states = unique(dfa_states)
    if length(dfa_states) > 1
        error("multiple dfa states for a single abstraction")
    end
    dfa_state = dfa_states[1]
    dfa_state
end

function add_abstraction_to_dfa(dfa, symbol, abstraction)
    if isnothing(dfa)
        return nothing
    end
    dfa = deepcopy(dfa)
    symbols = vcat(abstraction.dfa_metavars, abstraction.dfa_symvars)
    dfa[abstraction.dfa_root][symbol] = symbols
    dfa
end


function compress(original_corpus; iterations=3, dfa=nothing, kwargs...)
    corpus = original_corpus
    abstractions = Abstraction[]
    config = SearchConfig(; dfa=dfa, kwargs...)
    for i in 1:iterations
        println("===Iteration $i===")
        config.new_abstraction_name = Symbol("fn_$i")
        search_res = stitch_search(corpus, config)
        if isnothing(search_res)
            println("No more abstractions")
            break
        end
        search_res.abstraction.dfa_root = root_dfa_state(search_res)
        (rewritten, compressive, cumulative) = rewrite(search_res)
        corpus = rewritten
        dfa = add_abstraction_to_dfa(dfa, config.new_abstraction_name, search_res.abstraction)
        config = SearchConfig(; dfa=dfa, kwargs...)
        push!(abstractions, search_res.abstraction)
    end
    println("Total compression: ", size(original_corpus, config.size_by_symbol) / size(corpus, config.size_by_symbol), "x")
    return abstractions, corpus
end

function compress_imperative(original_corpus, dfa_path; kwargs...)
    compress(
        original_corpus;
        autoexpand_head = true,
        # only_match_semi = true,
        allow_single_task = false,
        verbose_best = false,
        dfa = load_dfa(dfa_path),
        kwargs...
    )
end

function load_corpus(file;truncate=nothing, kwargs...)
    json = JSON.parsefile(file)
    if !isnothing(truncate)
        json = json[1:truncate]
    end
    Corpus([Program(parse(SExpr, p),i,i) for (i,p) in enumerate(json)])
end


export uncurry_corpus, curry_corpus

function curry_corpus(corpus)
    Corpus([Program(curry(program.expr), program.id, program.task) for program in corpus.programs])
end

function uncurry_corpus(corpus)
    Corpus([Program(uncurry(program.expr), program.id, program.task) for program in corpus.programs])
end

function load_dfa(file)
    if isnothing(file)
        return nothing
    end
    json = JSON.parsefile(file)
    # dfa tells you given your current state and current head symbol, what is the vector of next states for each of your children
    dfa = Dict{Symbol, Dict{Symbol,Vector{Symbol}}}()
    for (state_str, transitions) in json
        state = Symbol(state_str)
        dfa[state] = Dict{Symbol,Vector{Symbol}}()
        for (head, next_states) in transitions
            dfa[state][Symbol(head)] = [Symbol(s) for s in next_states]
        end
    end
    dfa
end