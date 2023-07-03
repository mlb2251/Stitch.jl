
function rewrite(search_state::SearchState) :: Tuple{Corpus,Float32,Float32}

    cumulative_utility = bottom_up_utility(search_state);

    rewritten_programs = rewrite_program.(search_state.corpus.programs, search_state)
    rewritten = Corpus(rewritten_programs)

    compressive_utility = size(search_state.corpus) - size(rewritten)

    # @show rewritten
    if !isapprox(cumulative_utility, compressive_utility)
        # error("[$search_state] cumulative_utility != compressive_utility: $cumulative_utility != $compressive_utility")
        println("ERROR: [$search_state] cumulative_utility != compressive_utility: $cumulative_utility != $compressive_utility")
    end

    (rewritten, compressive_utility, cumulative_utility)
end

"""
Just copying Eqn 15 from https://arxiv.org/pdf/2211.16605.pdf

sets match.accept_rewrite and match.cumulative_utility
"""
function bottom_up_utility(search_state::SearchState) :: Float32
    # reset all utilities to NaN
    for expr in search_state.all_nodes
        expr.match.cumulative_utility = NaN32
        expr.match.is_active = false
    end

    for match in search_state.matches
        match.is_active = true
    end

    # special case the identity abstraction (\x. x) since it has a self loop dependency in terms of utility calculation
    # since you can rewrite X -> (identity X) -> (identity (identity X)) -> ... as you infintely rewrite the argument
    if is_identity_abstraction(search_state)
        for expr in search_state.all_nodes
            expr.match.cumulative_utility = 0.
            expr.match.accept_rewrite = false
        end
        return 0.
    end

    for expr in search_state.all_nodes
        reject_util = sum(child -> child.match.cumulative_utility, expr.children, init=0.)
        accept_util = if !expr.match.is_active 0. else expr.match.local_utility + sum(arg -> arg.match.cumulative_utility, expr.match.unique_args, init=0.) end
        expr.match.cumulative_utility = max(reject_util, accept_util)
        expr.match.accept_rewrite = accept_util > reject_util + .0001 # slightly in favor of rejection to avoid floating point rounding errors in the approximate equality case
        expr.match.cumulative_utility >= 0 || error("cumulative utility should be non-negative, not $(expr.match.cumulative_utility)");

        # expr.match.accept_rewrite && println("accepted rewrite at $expr with cumulative utility $(expr.match.cumulative_utility) and local utility $(expr.match.local_utility)")
    end

    # Eqn 18 from https://arxiv.org/pdf/2211.16605.pdf
    sum(programs -> minimum(p -> p.expr.match.cumulative_utility, programs), values(search_state.corpus.programs_by_task))
end

rewrite_program(program, search_state) = Program(rewrite_inner(program.expr, search_state), program.id, program.task)

function rewrite_inner(expr::SExpr, search_state::SearchState) :: SExpr
    # if cumulative utility <= 0 then there are no rewrites in this whole subtree
    expr.match.cumulative_utility > 0 || return copy(expr)

    if expr.match.accept_rewrite
        # do a rewrite
        children = [sexpr_leaf(search_state.config.new_abstraction_name)]
        for arg in expr.match.unique_args
            push!(children, rewrite_inner(arg, search_state))
        end
        if !isnothing(expr.match.continuation)
            push!(children, rewrite_inner(expr.match.continuation, search_state))
        end
        return sexpr_node(children)
    else
        # don't rewrite - just recurse
        return sexpr_node([rewrite_inner(child, search_state) for child in expr.children])
    end
end

