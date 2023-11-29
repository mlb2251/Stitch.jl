
function rewrite(search_state::SearchState) :: Tuple{Corpus,Float32,Float32}

    cumulative_utility = bottom_up_utility(search_state)

    rewritten_programs = rewrite_program.(search_state.corpus.programs, search_state)
    rewritten = Corpus(rewritten_programs)

    size_by_symbol = search_state.config.size_by_symbol
    corpus_compression_utility = size(search_state.corpus, size_by_symbol) - size(rewritten, size_by_symbol)
    abstraction_size_utility = -size(search_state.abstraction.body, size_by_symbol)
    additional_per_match_utility = (
        # adding 1 because that's already handled in the corpus compression utility by the fn_K symbol
        1 + search_state.config.application_utility_fixed
        # per-metavariable utility
        + search_state.config.application_utility_metavar * search_state.abstraction.arity
        # per-symvar utility
        + search_state.config.application_utility_symvar * search_state.abstraction.sym_arity
    ) * length(search_state.matches)
    compressive_utility = corpus_compression_utility + abstraction_size_utility + additional_per_match_utility

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
function bottom_up_utility(search_state::SearchState)::Float32

    rewrite_conflict_info = Dict(
        expr.metadata.id => RewriteConflictInfo(
            NaN32, false, false
        )
        for expr in search_state.all_nodes
    )

    for match in search_state.matches
        rewrite_conflict_info[match.expr.metadata.id].is_active = true
    end

    # special case the identity abstraction (\x. x) since it has a self loop dependency in terms of utility calculation
    # since you can rewrite X -> (identity X) -> (identity (identity X)) -> ... as you infintely rewrite the argument
    if is_identity_abstraction(search_state)
        for expr in search_state.all_nodes
            rewrite_conflict_info[expr.metadata.id].cumulative_utility = 0.0
            rewrite_conflict_info[expr.metadata.id].match.accept_rewrite = false
        end
        return 0.
    end

    for expr in search_state.all_nodes
        rci = rewrite_conflict_info[expr.metadata.id]

        reject_util = sum(child -> rewrite_conflict_info[child.metadata.id].cumulative_utility, expr.children, init=0.0)
        accept_util = if !rci.is_active
            0.0
        else
            expr.match.local_utility + sum(arg -> rewrite_conflict_info[arg.metadata.id].cumulative_utility, expr.match.unique_args, init=0.0)
        end
        rci.cumulative_utility = max(reject_util, accept_util)
        rci.accept_rewrite = accept_util > reject_util + 0.0001 # slightly in favor of rejection to avoid floating point rounding errors in the approximate equality case
        rci.cumulative_utility >= 0 || error("cumulative utility should be non-negative, not $(rewrite_conflict_info[expr.metadata.id].cumulative_utility)")

        # expr.match.accept_rewrite && println("accepted rewrite at $expr with cumulative utility $(expr.match.cumulative_utility) and local utility $(expr.match.local_utility)")
    end

    # Eqn 18 from https://arxiv.org/pdf/2211.16605.pdf
    corpus_util = sum(programs -> minimum(p -> rewrite_conflict_info[p.expr.metadata.id].cumulative_utility, programs), values(search_state.corpus.programs_by_task))
    corpus_util - size(search_state.abstraction.body, search_state.config.size_by_symbol)
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
        for sym in expr.match.sym_of_idx
            push!(children, sexpr_leaf(sym))
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

