

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



mutable struct SearchState
    abstraction::Abstraction
    corpus::Corpus

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

function init_search_state(corpus) :: SearchState
    abstraction = Abstraction(new_hole(nothing), 0)
    matches = hole_matches(corpus)
    SearchState(
        abstraction,
        corpus,
        [abstraction.body],
        matches,
        PossibleExpansion[],
        SExpr[],
        PossibleExpansion[],
        Match[],
        PossibleExpansion[],
    )
end


"""
Adds the set of expansions to whatever terminal or nonterminal is present at the match locations,
for example :app or :lambda or primitives or variables.
"""
function syntactic_expansions!(search_state)
    matches_of_sym = Dict{Symbol,Vector{Match}}() # optim: preallocate and reuse
    for match in search_state.matches
        sym = match.holes[end].head
        if haskey(matches_of_sym, sym)
            push!(matches_of_sym[sym], match)
        else
            matches_of_sym[sym] = [match]
        end
    end

    for (sym, matches) in matches_of_sym
        push!(search_state.expansions, PossibleExpansion(
            matches,
            SyntacticExpansion(sym, length(matches[1].holes[end].args)),
        ))
    end
end

function abstraction_expansions!(search_state, max_arity)
    # variable reuse
    for i in 0:search_state.abstraction.arity-1
        # todo implement and set fresh=false
    end
    if search_state.abstraction.arity < max_arity
        # fresh variable
        push!(search_state.expansions, PossibleExpansion(
            search_state.matches, # all the same matches
            AbstractionExpansion(search_state.abstraction.arity, true),
        ))
    end
end

function stitch_search(corpus, utility_fn, upper_bound_fn; max_arity=3, verbose=false, follow=nothing)
    best_util = Float32(0)
    best_abstraction = nothing
    search_state = init_search_state(corpus)

    needs_expansion = true

    # todo add arity zero here

    while true
        !verbose || println("abstraction: ", search_state.abstraction.body, " | matches: ", length(search_state.matches), " | expansions: ", length(search_state.expansions));

        if needs_expansion
            possible_expansions!(search_state, max_arity, upper_bound_fn, best_util)
            !verbose || println("possible_expansions!() -> ", length(search_state.expansions))
            needs_expansion = false
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

end

function possible_expansions!(search_state::SearchState, max_arity, upper_bound_fn, best_util)
    isempty(search_state.expansions) || error("expansions should be empty")

    syntactic_expansions!(search_state)
    abstraction_expansions!(search_state, max_arity)

    # filter out ones that dont pass bounds check
    filter!(e -> upper_bound_fn(search_state,e) > best_util, search_state.expansions)
end


"""
Saves current state to the stack, and reinitializes as a fresh state
"""
function expand_general!(search_state::SearchState, expansion)

    # pop hole
    hole = pop!(search_state.holes)
    hole.head === Symbol("??") || error("not a hole")
    push!(search_state.holes_stack,hole)

    # save state for backtracking
    push!(search_state.expansions_stack,search_state.expansions)
    push!(search_state.matches_stack,search_state.matches)
    push!(search_state.past_expansions,expansion)

    # fresh .expansions
    search_state.expansions = PossibleExpansion[]
    # set .matches properly
    search_state.matches = expansion.matches;

    # expand the state
    expand!(search_state, expansion, hole)

    all(match -> length(match.holes) == length(search_state.holes), search_state.matches) || error("mismatched number of holes");
end

function unexpand_general!(search_state::SearchState)

    hole = pop!(search_state.holes_stack);

    # pop the expansion to undo
    expansion = pop!(search_state.past_expansions)

    # unexpand - this should be an inverse to expand!()
    search_state.matches === expansion.matches || error("mismatched matches")
    unexpand!(search_state, expansion, hole)

    # restore other state
    search_state.expansions = pop!(search_state.expansions_stack)
    search_state.matches = pop!(search_state.matches_stack)
    push!(search_state.holes, hole)

    all(match -> length(match.holes) == length(search_state.holes), search_state.matches) || error("mismatched number of holes");
    # all(match -> length(match.args) == length(search_state.args), search_state.matches) || error("mismatched number of holes")    

end


function expand!(search_state, expansion::PossibleExpansion{SyntacticExpansion}, hole)
    
    # set the head symbol of the hole
    hole.head = expansion.data.head

    # add fresh holes to .args and also keep track of them in search_state.abstraction.holes
    for _ in 1:expansion.data.num_holes
        h = new_hole(hole)
        push!(hole.args, h)
        push!(search_state.holes, h)
    end
    # reverse holes so they go left to right
    @views reverse!(search_state.holes[end-expansion.data.num_holes+1:end])

    for match in search_state.matches
        hole = pop!(match.holes)
        length(hole.args) == expansion.data.num_holes || error("mismatched number of children to expand to at location: $(match.expr) with hole $hole for expansion $(expansion.data)")
        push!(match.holes_stack, hole)
        append!(match.holes, reverse(hole.args));
    end
end

function expand!(search_state, expansion::PossibleExpansion{AbstractionExpansion}, hole)

    hole.head = Symbol("#$(expansion.data.index)")

    if expansion.data.fresh
        search_state.abstraction.arity += 1
    end

    for match in search_state.matches
        hole = pop!(match.holes)
        push!(match.holes_stack, hole)
        push!(match.args, hole); # move the hole to be an argument
    end
end


function unexpand!(search_state, expansion::PossibleExpansion{SyntacticExpansion}, hole)
    
    # set the head symbol of the hole
    hole.head = Symbol("??")

    # pop from .args and search_state.holes
    for _ in 1:expansion.data.num_holes
        pop!(hole.args).head === pop!(search_state.holes).head === Symbol("??") || error("not a hole")
    end

    for match in search_state.matches
        hole = pop!(match.holes_stack) 
        length(hole.args) == expansion.data.num_holes || error("mismatched number of children to expand to; should be same though since expand!() checked this")
        push!(match.holes, hole)
        for _ in 1:expansion.data.num_holes
            pop!(match.holes)
        end
    end
end

function unexpand!(search_state, expansion::PossibleExpansion{AbstractionExpansion}, hole)

    hole.head = Symbol("??")
    if expansion.data.fresh
        search_state.abstraction.arity -= 1
    end

    for match in search_state.matches
        hole = pop!(match.holes_stack)
        push!(match.holes, hole)
        pop!(match.args) === hole || error("expected same hole");
    end
end

