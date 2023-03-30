function upper_bound_inf(search_state, expansion) :: Float32
    Inf32
end

"""
sum over match locations of size of match location
"""
function upper_bound_sum_subtree_sizes(search_state, expansion) :: Float32
    sum(m -> m.size, expansion.matches)
end


function expand_utility!(match, hole, expansion::PossibleExpansion{SyntacticExpansion})
    # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (abstraction size)
    match.local_utility += if expansion.data.num_holes == 0 1.0  else .01 end;
end

function expand_utility!(match, hole, expansion::PossibleExpansion{AbstractionExpansion})
    if expansion.data.fresh
        # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (application utility second term; cost_app * arity)
        match.local_utility -= .01;
    else
        # Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (multiuse utility; (usages-1)*cost(arg))
        match.local_utility += match.holes[end].data.size;
    end
end

# Eqn 12: https://arxiv.org/pdf/2211.16605.pdf (application utility first term; -cost_t(t_A))
local_utility_init() = -1.0


"""
size*matches utility
"""
function utility_size_time_matches(search_state) :: Float32
    (size_no_abstraction_var(search_state.abstraction.body) - 1 - .01 * search_state.abstraction.arity) * length(search_state.matches)
end


function utility_rewrite(search_state) :: Float32
    if is_identity_abstraction(search_state)
        return 0
    end

    rewritten = rewrite(search_state)
    size(search_state.corpus) - size(rewritten)
end

is_identity_abstraction(search_state) = length(search_state.past_expansions) == 1 && isa(search_state.past_expansions[1].data, AbstractionExpansion)

# function utility_rewrite_local(search_state, match)