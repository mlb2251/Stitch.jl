

function possible_expansions!(search_state)
    isempty(search_state.expansions) || error("expansions should be empty")

    syntactic_expansions!(search_state)
    abstraction_expansions!(search_state)
    symbol_expansions!(search_state)

    # sort!(search_state.expansions, by=e -> length(e.matches)*e.matches[1].local_utility)
    # sort!(search_state.expansions, by=e -> upper_bound_fn(search_state,e))

    # if tracking is turned on we'll defer the pruning till later when we can more easily indicate things being pruned
    !isnothing(search_state.config.track) && return

    # filter out ones that dont pass bounds check
    # filter!(e -> upper_bound_fn(search_state,e) > best_util, search_state.expansions);
end


"""
Adds the set of expansions to whatever terminal or nonterminal is present at the match locations,
for example :app or :lambda or primitives or variables.
"""
function syntactic_expansions!(search_state)
    matches_of_leaf = Dict{Symbol,Vector{Match}}() # can't prealloc - these must be fresh array objects that must persist and cant be cleared after this!
    matches_of_node = Dict{Int,Vector{Match}}()
    for match in search_state.matches
        if is_leaf(match.holes[end])
            # leaf case
            leaf = match.holes[end].leaf
            startswith(string(leaf), "&") && continue

            if haskey(matches_of_leaf, leaf)
                push!(matches_of_leaf[leaf], match)
            else
                matches_of_leaf[leaf] = [match]
            end
        else
            # node case - group with other nodes that have same number of children
            childcount = length(match.holes[end].children)
            if haskey(matches_of_node, childcount)
                push!(matches_of_node[childcount], match)
            else
                matches_of_node[childcount] = [match]
            end
        end
    end

    for (leaf, matches) in matches_of_leaf
        isempty(matches) && continue
        push!(search_state.expansions, PossibleExpansion(
            matches,
            SyntacticLeafExpansion(leaf),
        ))
    end
    for (childcount, matches) in matches_of_node
        isempty(matches) && continue
        push!(search_state.expansions, PossibleExpansion(
            matches,
            SyntacticNodeExpansion(childcount),
        ))
    end
end


function symbol_expansions!(search_state)
    matches_of_idx = Dict{Int,Vector{Match}}()
    for match in search_state.matches
        is_leaf(match.holes[end]) || continue
        sym = match.holes[end].leaf
        if !startswith(string(sym), "&") # this is not a symbol
            continue
        end


        idx = get(match.idx_of_sym, sym, length(match.sym_of_idx) + 1)

        if haskey(matches_of_idx, idx)
            push!(matches_of_idx[idx], match)
        else
            matches_of_idx[idx] = [match]
        end
    end

    for (idx, matches) in matches_of_idx
        if isempty(matches) continue end
        push!(search_state.expansions, PossibleExpansion(
            matches,
            SymbolExpansion(idx),
        ))
    end

end

function abstraction_expansions!(search_state)
    # note: abstracting out a symbol or subtree containing a symbol is okay because you can pass
    # a symbol in just fine bc ur doing it at the call site so it's bound correctly already.

    isnothing(search_state.holes[end]) && return # no identity abstraction allowed

    # variable reuse
    for i in 0:search_state.abstraction.arity-1
        matches = copy(search_state.matches)
        filter!(m -> m.holes[end].match.struct_hash == m.unique_args[i+1].match.struct_hash, matches)
        # matches = [m for m in search_state.matches if m.holes[end].match.struct_hash == m.unique_args[i+1].match.struct_hash]
        if isempty(matches) continue end

        push!(search_state.expansions, PossibleExpansion(
            matches,
            AbstractionExpansion(i, false),
        ))
    end
    if search_state.abstraction.arity < search_state.config.max_arity
        # fresh variable
        push!(search_state.expansions, PossibleExpansion(
            search_state.matches, # all the same matches
            AbstractionExpansion(search_state.abstraction.arity, true),
        ))
    end
end

function continuation_expansions!(search_state)

    node = search_state.holes[end]
    isnothing(node.parent) && return # no identity abstraction allowed

    while !isnothing(node.parent)
        (parent,i) = node.parent
        # check that our parent is a semicolon and we are the righthand child
        # so in particular parent == (semi _ node)
        i == 3 || return
        parent.children[1] === :semi || return
        node = parent
    end

    isnothing(search_state.continuation) || error("shouldn't be possible to have two continuation expansions!")

    push!(search_state.expansions, PossibleExpansion(
        search_state.matches, # all the same matches
        ContinuationExpansion(),
    ))
end


"""
Saves current state to the stack, and reinitializes as a fresh state
"""
function expand_general!(search_state, expansion)

    # pop hole
    hole = pop!(search_state.holes)
    is_hole(hole) || error("not a hole")
    push!(search_state.holes_stack,hole)

    # save state for backtracking
    push!(search_state.expansions_stack,search_state.expansions)
    push!(search_state.matches_stack,search_state.matches)
    push!(search_state.past_expansions,expansion)

    # fresh .expansions
    search_state.expansions = PossibleExpansion[]
    # set .matches properly
    search_state.matches = expansion.matches;

    for match in expansion.matches
        push!(match.local_utility_stack, match.local_utility)
        expand_utility!(match, hole, expansion)
    end

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

    for match in search_state.matches
        match.local_utility = pop!(match.local_utility_stack)
    end

    # restore other state
    search_state.expansions = pop!(search_state.expansions_stack)
    search_state.matches = pop!(search_state.matches_stack)
    push!(search_state.holes, hole)

    all(match -> length(match.holes) == length(search_state.holes), search_state.matches) || error("mismatched number of holes");
    # all(match -> length(match.args) == length(search_state.args), search_state.matches) || error("mismatched number of holes")    

end


function expand!(search_state, expansion::PossibleExpansion{SyntacticLeafExpansion}, hole)
    
    # set the head symbol of the hole
    hole.leaf = expansion.data.leaf

    for match in search_state.matches
        # pop next hole and save it for future backtracking
        hole = pop!(match.holes)
        push!(match.holes_stack, hole)
        @assert is_leaf(hole)
    end
end

function expand!(search_state, expansion::PossibleExpansion{SyntacticNodeExpansion}, hole)

    # make it no longer a leaf
    hole.leaf = nothing
    
    # add fresh holes to .args and also keep track of them in search_state.abstraction.holes
    for i in 1:expansion.data.num_holes
        h = new_hole((hole,i))
        push!(hole.children, h)
        push!(search_state.holes, h)
    end

    # reverse holes so they go left to right
    # @views reverse!(search_state.holes[end-expansion.data.num_holes+1:end])

    for match in search_state.matches
        # pop next hole and save it for future backtracking
        hole = pop!(match.holes)
        length(hole.children) == expansion.data.num_holes || error("mismatched number of children to expand to at location: $(match.expr) with hole $hole for expansion $(expansion.data)")
        push!(match.holes_stack, hole)

        # add all the children of the hole as new holes
        append!(match.holes, hole.children)
    end
end



function expand!(search_state, expansion::PossibleExpansion{AbstractionExpansion}, hole)

    hole.leaf = Symbol("#$(expansion.data.index)")

    if expansion.data.fresh
        search_state.abstraction.arity += 1
    end

    for match in search_state.matches
        hole = pop!(match.holes)
        push!(match.holes_stack, hole)
        push!(match.all_args, hole);
        if expansion.data.fresh
            push!(match.unique_args, hole); # move the hole to be an argument
        end
    end
end

function expand!(search_state, expansion::PossibleExpansion{SymbolExpansion}, hole)
    
    # set the head symbol of the hole
    hole.leaf = Symbol("%$(expansion.data.idx)")

    for match in search_state.matches
        # pop next hole and save it for future backtracking
        hole = pop!(match.holes)
        push!(match.holes_stack, hole)

        @assert string(hole.leaf)[1] == '&'

        if length(match.sym_of_idx) < expansion.data.idx
            # this is a new symbol
            push!(match.sym_of_idx, hole.leaf)
            match.idx_of_sym[hole.leaf] = expansion.data.idx
            push!(match.idx_is_fresh,true)
        else
            push!(match.idx_is_fresh,false)
        end

    end
end

function expand!(search_state, expansion::PossibleExpansion{ContinuationExpansion}, hole)

    hole.leaf = Symbol("#continuation")

    for match in search_state.matches
        hole = pop!(match.holes)
        push!(match.holes_stack, hole)
        @assert isnothing(match.continuation)
        match.continuation = hole;
    end
end



function unexpand!(search_state, expansion::PossibleExpansion{SyntacticLeafExpansion}, hole)
    
    # set the head symbol of the hole
    hole.leaf = SYM_HOLE

    for match in search_state.matches
        hole = pop!(match.holes_stack) 
        push!(match.holes, hole)
    end
end

function unexpand!(search_state, expansion::PossibleExpansion{SyntacticNodeExpansion}, hole)
    hole.leaf = SYM_HOLE

    # pop from .args and search_state.holes
    for _ in 1:expansion.data.num_holes
        pop!(hole.children).leaf === pop!(search_state.holes).leaf === SYM_HOLE || error("not a hole")
    end

    for match in search_state.matches
        for _ in 1:expansion.data.num_holes
            pop!(match.holes)
        end

        hole = pop!(match.holes_stack) 
        length(hole.children) == expansion.data.num_holes || error("mismatched number of children to expand to; should be same though since expand!() checked this")
        push!(match.holes, hole)
    end
end


function unexpand!(search_state, expansion::PossibleExpansion{AbstractionExpansion}, hole)

    hole.leaf = SYM_HOLE
    if expansion.data.fresh
        search_state.abstraction.arity -= 1
    end

    for match in search_state.matches
        hole = pop!(match.holes_stack)
        push!(match.holes, hole)
        if expansion.data.fresh
            pop!(match.unique_args) === hole || error("expected same hole");
        end
        pop!(match.all_args) === hole || error("expected same hole");
    end
end

function unexpand!(search_state, expansion::PossibleExpansion{SymbolExpansion}, hole)
    
    # set the head symbol of the hole
    hole.leaf = SYM_HOLE

    for match in search_state.matches
        hole = pop!(match.holes_stack) 
        push!(match.holes, hole)

        if pop!(match.idx_is_fresh)
            pop!(match.sym_of_idx)
            delete!(match.idx_of_sym, hole.leaf)
        end

    end
end

function unexpand!(search_state, expansion::PossibleExpansion{ContinuationExpansion}, hole)

    hole.leaf = SYM_HOLE

    for match in search_state.matches
        hole = pop!(match.holes_stack)
        push!(match.holes, hole)
        @assert match.continuation === hole
        match.continuation = nothing;
    end
end



# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function strictly_dominated(search_state)
    redundant_arg_elim(search_state) || arg_capture(search_state)
end

# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function redundant_arg_elim(search_state)
    search_state.config.no_opt_redundant_args && return false
    for i in 1:search_state.abstraction.arity
        for j in i+1:search_state.abstraction.arity
            if all(match -> match.unique_args[i].match.struct_hash == match.unique_args[j].match.struct_hash, search_state.matches)
                return true
            end
        end
    end
    false
end

# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function arg_capture(search_state)
    search_state.config.no_opt_arg_capture && return false
    for i in 1:search_state.abstraction.arity
        first_match = search_state.matches[1].unique_args[i].match.struct_hash;
        if all(match -> match.unique_args[i].match.struct_hash == first_match, search_state.matches)
            return true
        end
    end 
    false
end

function is_single_task(search_state)
    first = search_state.matches[1].program.task
    all(match -> match.program.task == first, search_state.matches)
end

mutable struct SamplingProcessor{F <: Function}
    keep_frac::Float32
    score::F
end

using StatsBase
function process_expansions!(search_state)
    weights = search_state.expansion_processor.score.(search_state.expansions, search_state)
    num_samples = max(Int(round(length(search_state.expansions) * processor.keep_frac)), min(2, length(search_state.expansions)))
    search_state.expansions = sample(search_state.expansions, Weights(weights), num_samples, replace=false);
end
