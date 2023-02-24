

function possible_expansions!(search_state, max_arity, upper_bound_fn, best_util)
    isempty(search_state.expansions) || error("expansions should be empty")

    syntactic_expansions!(search_state)
    abstraction_expansions!(search_state, max_arity)

    # filter out ones that dont pass bounds check
    filter!(e -> upper_bound_fn(search_state,e) > best_util, search_state.expansions)
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


"""
Saves current state to the stack, and reinitializes as a fresh state
"""
function expand_general!(search_state, expansion)

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
    # @views reverse!(search_state.holes[end-expansion.data.num_holes+1:end])

    for match in search_state.matches
        hole = pop!(match.holes)
        length(hole.args) == expansion.data.num_holes || error("mismatched number of children to expand to at location: $(match.expr) with hole $hole for expansion $(expansion.data)")
        push!(match.holes_stack, hole)
        append!(match.holes, hole.args);
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
        for _ in 1:expansion.data.num_holes
            pop!(match.holes)
        end

        hole = pop!(match.holes_stack) 
        length(hole.args) == expansion.data.num_holes || error("mismatched number of children to expand to; should be same though since expand!() checked this")
        push!(match.holes, hole)
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