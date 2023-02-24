function upper_bound_inf(search_state, expansion) :: Float32
    Inf32
end

"""
sum over match locations of size of match location
"""
function upper_bound_sum_subtree_sizes(search_state, expansion) :: Float32
    sum(m -> m.cost)
end

"""
size*matches utility
"""
function utility_size_time_matches(search_state) :: Float32
    (size_no_abstraction_var(search_state.abstraction.body) - 1) * length(search_state.matches)
end
