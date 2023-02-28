

mutable struct Program
    expr::SExpr
    id::Int
    task::Int
end

mutable struct Corpus
    programs::Vector{Program}
end

mutable struct Match
    expr::SExpr # pointer to subtree in original corpus
    args::Vector{SExpr} # pointers to first instance of each arg within subtree ie args[1] is #0
    holes::Vector{SExpr} # pointers to holes within subtree
    holes_stack::Vector{SExpr} # expanded holes
    program::Program # which program this subtree appears in
    size::Float32
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

struct AbstractionExpansion <: Expansion
    index::Int
    fresh::Bool
end

mutable struct Abstraction
    body::SExpr
    arity::Int
end

mutable struct Stats
    expansions::Int
    completed::Int

    comparable_worklist_steps::Int
end



mutable struct SearchState
    abstraction::Abstraction
    corpus::Corpus
    stats::Stats
    new_abstraction_name::Symbol

    holes::Vector{SExpr}
    matches::Vector{Match} 
    expansions::Vector{PossibleExpansion}

    holes_stack::Vector{SExpr}
    expansions_stack::Vector{Vector{PossibleExpansion}}
    matches_stack::Vector{Vector{Match}}
    past_expansions::Vector{PossibleExpansion}
end


function hole_matches(corpus) :: Vector{Match}
    matches = Match[]
    for program in corpus.programs
        for expr in subexpressions(program.expr)
            match = Match(expr, SExpr[], [expr], SExpr[], program, size(expr))
            push!(matches, match)
        end
    end
    matches
end

function init_search_state(corpus, new_abstraction_name) :: SearchState
    abstraction = Abstraction(new_hole(nothing), 0)
    matches = hole_matches(corpus)
    SearchState(
        abstraction,
        corpus,
        Stats(0,0,0),
        new_abstraction_name,
        [abstraction.body],
        matches,
        PossibleExpansion[],
        SExpr[],
        PossibleExpansion[],
        Match[],
        PossibleExpansion[],
    )
end



function stitch_search(corpus, utility_fn, upper_bound_fn; max_arity=3, verbose=false, follow=nothing, new_abstraction_name=nothing)

    if isnothing(new_abstraction_name)
        new_abstraction_name = gensym("f")
    end

    best_util = Float32(0)
    best_abstraction = nothing
    search_state = init_search_state(corpus, new_abstraction_name)

    needs_expansion = true

    # todo add arity zero here

    while true
        if needs_expansion
            !verbose || println("abstraction: ", search_state.abstraction.body,
            " | matches: ", length(search_state.matches),
            " | expansions: ", length(search_state.expansions));
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
            continue # skip - worse than best so far
        end
        
        expand_general!(search_state, expansion)

        search_state.stats.expansions += 1

        if !isnothing(follow)
            body = string(search_state.abstraction.body)
            prefix = split(body, "??")[1]
            if !startswith(follow, prefix)
                continue
            end
        end

        # !verbose || println("expanded with: ", expansion.data)

        # are we done?
        if isempty(search_state.holes)
            search_state.stats.completed += 1
            !verbose || println("completed: ", search_state.abstraction.body, " with utility ", utility_fn(search_state), " used in $(length(search_state.matches)) places")
            # eval util and possibly update best util
            util = utility_fn(search_state)
            if util > best_util
                best_util = util
                best_abstraction = deepcopy(search_state.abstraction)
                println("new best: ", search_state.abstraction.body, " with utility ", best_util, " used in $(length(search_state.matches)) places")
            end
            continue
        end

        needs_expansion = true
    end


    println("Best abstraction: ", best_abstraction.body, " with utility ", best_util);
    println(search_state.stats);

end


