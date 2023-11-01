
function symbol_size(sym::Symbol, size_by_symbol::SizeBySymbol)
    if sym in keys(size_by_symbol.symbol_to_size)
        return size_by_symbol.symbol_to_size[sym]
    else
        return 1.0
    end
end

function symbol_size(sym::Symbol, size_by_symbol::Nothing)
    1.0
end

function upper_bound_inf(search_state, expansion) :: Float32
    Inf32
end

"""
sum over match locations of size of match location
"""
function upper_bound_sum_subtree_sizes(search_state, expansion=nothing) :: Float32
    matches = if isnothing(expansion) search_state.matches else expansion.matches end
    sum(m -> m.size, matches)
end

"""
Same as summing over sizes of subtrees, but not doublecounting matches within children.
"""
function upper_bound_with_conflicts(search_state, expansion=nothing) :: Float32
    matches = if isnothing(expansion) search_state.matches else expansion.matches end
    issorted(matches, by=m -> m.id) || error("matches is not sorted")

    bound = 0.
    offset = length(matches)

    while true
        bound += matches[offset].size
        # since matches is sorted in child-first order, children are always to the left of parents. We
        # can use .num_nodes to see how many children a match has (how big the subtree is) and skip over that many
        # things.
        next_id = matches[offset].id - matches[offset].num_nodes
        next_id == 0 && break
        search_state.all_nodes[next_id].match.id == next_id || error("all_nodes is not in the right order")

        # common case: stepping one to the left in the matches array doesnt result
        # in a child of the previous match, so we dont need to run a binary search since
        # this is what it would return anyways
        offset -= 1
        offset == 0 && break
        matches[offset].id <= next_id && continue

        # rarer case: run binary search to find the rightmost non-child of the previous match
        offset = searchsortedlast(matches, search_state.all_nodes[next_id].match, by=m -> m.id)
        offset == 0 && break
    end
    bound
end

function expand_utility!(match, hole, expansion::PossibleExpansion{SymbolExpansion})
    # future direction: here we think of symbols as being zero cost to pass in ie 1.0 utility (as if we deleted their)
    # node from the corpus.
    if expansion.data.fresh
        match.local_utility += 0;
    else
        match.local_utility += 1;
    end
end


function expand_utility!(match, hole, expansion::PossibleExpansion{SyntacticLeafExpansion})
    # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (abstraction size)
    match.local_utility += symbol_size(expansion.data.leaf, match.size_by_symbol)
end

function expand_utility!(match, hole, expansion::PossibleExpansion{SyntacticNodeExpansion})
    # let it be zero?
    # match.local_utility += 0.;
    if expansion.data.head !== :no_expand_head
        match.local_utility += symbol_size(expansion.data.head, match.size_by_symbol)
    end
    nothing
end

function expand_utility!(match, hole, expansion::PossibleExpansion{AbstractionExpansion})
    if expansion.data.fresh
        # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (application utility second term; cost_app * arity)
        # note: commented out with switch away from application penalty
        # match.local_utility -= .01;

        # actually do nothing here
    else
        # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (multiuse utility; (usages-1)*cost(arg))
        match.local_utility += match.holes[end].match.size;
    end
    nothing
end

function expand_utility!(match, hole, expansion::PossibleExpansion{ContinuationExpansion})
    # zero
end

# Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (application utility first term; -cost_t(t_A))
local_utility_init() = -1.0


"""
size*matches utility
"""
# function utility_size_time_matches(search_state) :: Float32
#     (size_no_abstraction_var(search_state.abstraction.body) - 1 - .01 * search_state.abstraction.arity) * length(search_state.matches)
# end


function utility_rewrite(search_state) :: Float32
    if is_identity_abstraction(search_state)
        return 0
    end

    rewritten = rewrite(search_state)
    size_by_symbol = search_state.config.size_by_symbol
    size(search_state.corpus, size_by_symbol) - size(rewritten, size_by_symbol)
end

is_identity_abstraction(search_state) = length(search_state.past_expansions) == 1 && isa(search_state.past_expansions[1].match, AbstractionExpansion)
