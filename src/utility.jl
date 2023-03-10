function upper_bound_inf(search_state, expansion) :: Float32
    Inf32
end

"""
sum over match locations of size of match location
"""
function upper_bound_sum_subtree_sizes(search_state, expansion) :: Float32
    sum(m -> m.size, expansion.matches)
end

"""
size*matches utility
"""
function utility_size_time_matches(search_state) :: Float32
    (size_no_abstraction_var(search_state.abstraction.body) - 1 - .01 * search_state.abstraction.arity) * length(search_state.matches)
end

function utility_rewrite(search_state) :: Float32
    if length(search_state.past_expansions) == 1 && isa(search_state.past_expansions[1].data, AbstractionExpansion)
        return 0
    end

    rewritten = rewrite(search_state)
    size(search_state.corpus) - size(rewritten)
end