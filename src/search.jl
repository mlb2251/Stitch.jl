
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
    struct_hash::Int

    # Tracks Eqn 12: https://arxiv.org/pdf/2211.16605.pdf
    local_utility::Float32
    
    cumulative_utility::Float32
    accept_rewrite::Bool
    is_active::Bool

    Match(expr, program) = new(expr, [], [], [expr], [], [], program, size(expr), struct_hash(expr), local_utility_init(), NaN32, false, false)
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
    for program in corpus.programs
        for expr in subexpressions(program.expr)
            match = Match(expr, program)
            expr.data = match
            push!(matches, match)
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


function stitch_search(corpus, upper_bound_fn, new_abstraction_name; max_arity=2, verbose=false, track=nothing, follow=false)

    best_util = Float32(0)
    best_abstraction = nothing
    search_state = SearchState(corpus, new_abstraction_name, track)

    

    needs_expansion = true

    # todo add arity zero here

    while true
        if needs_expansion
            !verbose || printstyled(search_state, "\n", color=:yellow);
            possible_expansions!(search_state, max_arity, upper_bound_fn, best_util)
            !verbose || println("possible_expansions!() -> ", length(search_state.expansions), " ", [e.data for e in search_state.expansions])
            needs_expansion = false
            search_state.stats.comparable_worklist_steps += 1
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
        if upper_bound_fn(search_state,expansion) <= best_util
            is_tracked_pruned(search_state, expansion=expansion, message="$(@__FILE__):$(@__LINE__) - upper bound $(upper_bound_fn(search_state,expansion)) <= best util $best_util")
            continue # skip - worse than best so far
        end
        
        expand_general!(search_state, expansion)

        search_state.stats.expansions += 1

        if is_tracked(search_state)
            printstyled("TRACK: ", search_state.abstraction.body, "\n", color=:green, bold=true)
        elseif follow && !is_tracked(search_state)
            continue
        end

        # !verbose || println("expanded with: ", expansion.data)

        # are we done?
        if isempty(search_state.holes)            
            search_state.stats.completed += 1

            # (rewritten, compressive, cumulative) = rewrite(search_state)
            # @show rewritten compressive cumulative
            
            !verbose || println("completed: ", search_state.abstraction.body, " with utility ", bottom_up_utility(search_state), " used in $(length(search_state.matches)) places")
            
            # cheaply upper bounded version of util that uses no conflict resolution
            approx_util = sum(match -> match.local_utility, search_state.matches)
            if approx_util <= best_util
                continue # skip - worse than best so far
            end

            # eval util and possibly update best util
            util = bottom_up_utility(search_state)    
            
            if util > best_util
                best_util = util
                best_abstraction = copy(search_state.abstraction)
                printstyled("new best: ", search_state.abstraction.body, " with utility ", best_util, " used in $(length(search_state.matches)) places\n", color=:green)
            end
            continue
        end

        needs_expansion = true
    end

    if isnothing(best_abstraction)
        println("No abstractions found")
    else 
        println("Best abstraction: ", best_abstraction.body, " with utility ", best_util);
        (rewritten, compressive, cumulative) = rewrite(search_state)
    end
    println(search_state.stats);

end


