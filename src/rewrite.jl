
mutable struct RewriteConflictInfo
    # handling rewrite conflicts. When the same abstraction can be used in two overlapping places, we need to pick one.
    # e.g., program = (foo (foo (foo x))). abstraction = (foo (foo #0)). abstraction can either match
    # as (fn_1 (foo x)) or (foo (fn_1 x)). We need to pick one.

    # Simple bottom-up dynamic programming to figure out which is best
    cumulative_utility::Float32
    accept_rewrite::Bool
    is_active::Bool
end

const MultiRewriteConflictInfo = Dict{Int64, RewriteConflictInfo}

function rewrite(search_state::SearchState) :: Tuple{Corpus,Float32,Float32}

    cumulative_utility, rci = collect_rci(search_state)

    rewritten_programs = [rewrite_program(program, search_state, rci) for program in search_state.corpus.programs]
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
"""
function collect_rci(search_state::SearchState)::Tuple{Float64, MultiRewriteConflictInfo}

    rcis = Dict(
        expr.metadata.id => RewriteConflictInfo(
            NaN32, false, false
        )
        for expr in search_state.all_nodes
    )

    for match in search_state.matches
        rcis[expr_of_match(match).metadata.id].is_active = true
    end

    # special case the identity abstraction (\x. x) since it has a self loop dependency in terms of utility calculation
    # since you can rewrite X -> (identity X) -> (identity (identity X)) -> ... as you infintely rewrite the argument
    if is_identity_abstraction(search_state)
        for expr in search_state.all_nodes
            rcis[expr.metadata.id].cumulative_utility = 0.0
            rcis[expr.metadata.id].accept_rewrite = false
        end
        return 0.
    end

    for expr in search_state.all_nodes
        rci = rcis[expr.metadata.id]

        reject_util = sum(child -> rcis[child.metadata.id].cumulative_utility, expr.children, init=0.0)
        accept_util = if !rci.is_active
            0.0
        else
            expr.match.local_utility + sum(arg -> rcis[arg.metadata.id].cumulative_utility, expr.match.unique_args, init=0.0)
        end
        rci.cumulative_utility = max(reject_util, accept_util)
        rci.accept_rewrite = accept_util > reject_util + 0.0001 # slightly in favor of rejection to avoid floating point rounding errors in the approximate equality case
        rci.cumulative_utility >= 0 || error("cumulative utility should be non-negative, not $(rcis[expr.metadata.id].cumulative_utility)")

        # expr.match.accept_rewrite && println("accepted rewrite at $expr with cumulative utility $(expr.match.cumulative_utility) and local utility $(expr.match.local_utility)")
    end

    # Eqn 18 from https://arxiv.org/pdf/2211.16605.pdf
    corpus_util = sum(programs -> minimum(p -> rcis[p.expr.metadata.id].cumulative_utility, programs), values(search_state.corpus.programs_by_task))
    util = corpus_util - size(search_state.abstraction.body, search_state.config.size_by_symbol)
    return util, rcis
end

function bottom_up_utility(search_state :: SearchState) :: Float64
    util, _ = collect_rci(search_state)
    return util
end

rewrite_program(program, search_state, rcis) = Program(rewrite_inner(program.expr, search_state, rcis), program.id, program.task)

function rewrite_inner(expr::SExpr, search_state::SearchState, rcis::MultiRewriteConflictInfo) :: SExpr
    # if cumulative utility <= 0 then there are no rewrites in this whole subtree
    rci = rcis[expr.metadata.id]
    rci.cumulative_utility > 0 || return copy(expr)

    if rci.accept_rewrite
        # do a rewrite
        children = [sexpr_leaf(search_state.config.new_abstraction_name)]
        for arg in expr.match.unique_args
            push!(children, rewrite_inner(arg, search_state, rcis))
        end
        for sym in expr.match.sym_of_idx
            push!(children, sexpr_leaf(sym))
        end
        if !isnothing(expr.match.continuation)
            push!(children, rewrite_inner(expr.match.continuation, search_state, rcis))
        end
        return sexpr_node(children)
    else
        # don't rewrite - just recurse
        return sexpr_node([rewrite_inner(child, search_state, rcis) for child in expr.children])
    end
end

