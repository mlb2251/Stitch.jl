
function rewrite(search_state::SearchState) :: Tuple{Corpus,Float32,Float32}

    cumulative_utility = bottom_up_utility(search_state);

    rewritten_programs = rewrite_program.(search_state.corpus.programs, search_state)
    rewritten = Corpus(rewritten_programs)

    compressive_utility = size(rewritten) - size(search_state.corpus)

    cumulative_utility == compressive_utility || error("expected utility $expected_utility does not match actual utility $actual_utility")

    (rewritten, compressive_utility, cumulative_utility)
end

"""
Just copying Eqn 15 from https://arxiv.org/pdf/2211.16605.pdf

sets match.accept_rewrite and match.cumulative_utility
"""
function bottom_up_utility(search_state) :: Float32
    for expr in search_state.all_nodes
        expr.data.cumulative_utility = NaN32
    end

    # special case the identity abstraction (\x. x) since it has a self loop dependency in terms of utility calculation
    if is_identity_abstraction(search_state)
        for expr in search_state.all_nodes
            expr.data.cumulative_utility = 0.
            expr.data.accept_rewrite = false
        end
        return 0.
    end

    for expr in search_state.all_nodes
        reject_util = sum(child -> child.data.cumulative_utility, expr.args, init=0.)
        accept_util = expr.data.local_utility + sum(arg -> arg.data.cumulative_utility, expr.data.unique_args, init=0.)
        expr.data.cumulative_utility = max(reject_util, accept_util)
        expr.data.accept_rewrite = accept_util > reject_util
        expr.data.cumulative_utility >= 0 || error("cumulative utility should be non-negative, not $(expr.data.cumulative_utility)");
    end

    # Eqn 18 from https://arxiv.org/pdf/2211.16605.pdf
    sum(minimum.(p -> p.expr.data.cumulative_utility, values(search_state.corpus.programs_by_task)))
end

rewrite_program(program, search_state) = Program(rewrite_inner(program.expr, search_state), program.id, program.task)

function rewrite_inner(expr, search_state) :: SExpr
    # if cumulative utility <= 0 then there are no rewrites in this whole subtree
    expr.data.cumulative_utility > 0 || return copy(expr)

    if expr.data.accept_rewrite
        # do a rewrite
        return curried_application(search_state.new_abstraction_name, [rewrite_inner(copy(arg), search_state) for arg in match.unique_args])
    else
        # don't rewrite - just recurse
        return SExpr(expr.head, [rewrite_inner(copy(arg), search_state) for arg in expr.args])
    end
end

