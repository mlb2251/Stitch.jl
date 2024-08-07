
mutable struct RewriteConflictInfo{M}
    # handling rewrite conflicts. When the same abstraction can be used in two overlapping places, we need to pick one.
    # e.g., program = (foo (foo (foo x))). abstraction = (foo (foo #0)). abstraction can either match
    # as (fn_1 (foo x)) or (foo (fn_1 x)). We need to pick one.

    # Simple bottom-up dynamic programming to figure out which is best
    cumulative_utility::Float32
    accept_rewrite::Bool
    rci_match_possibilities::Union{M,Nothing}
    rci_match::Union{Match,Nothing}
end

const MultiRewriteConflictInfo{M} = Dict{Int64,RewriteConflictInfo{M}}

function rewrite(search_state::SearchState)::Tuple{Corpus,Float32,Float32}

    cumulative_utility, rci = collect_rci(search_state)

    rewritten_programs = [rewrite_program(program, search_state, rci) for program in search_state.corpus.programs]
    rewritten = Corpus(rewritten_programs)

    size_by_symbol = search_state.config.size_by_symbol
    corpus_compression_utility = size(search_state.corpus, size_by_symbol) - size(rewritten, size_by_symbol)
    abstraction_size_utility = -search_state.abstraction.body_size
    additional_per_match_utility = (
        # adding 1 because that's already handled in the corpus compression utility by the fn_K symbol
        1 + search_state.config.application_utility_fixed
        # per-metavariable utility
        + search_state.config.application_utility_metavar * search_state.abstraction.arity
        # per-symvar utility
        + search_state.config.application_utility_symvar * search_state.abstraction.sym_arity
        # per-choice-var utility
        + search_state.config.application_utility_choicevar * search_state.abstraction.choice_arity
    ) * length(search_state.matches)
    compressive_utility = corpus_compression_utility + abstraction_size_utility + additional_per_match_utility

    # @show rewritten
    if !isapprox(cumulative_utility, compressive_utility, rtol=1e-5)
        msg = "ERROR: [$search_state] cumulative_utility != compressive_utility: $cumulative_utility != $compressive_utility"
        if search_state.config.strict
            error(msg)
        else
            println(msg)
        end
    end

    (rewritten, compressive_utility, cumulative_utility)
end

"""
Just copying Eqn 15 from https://arxiv.org/pdf/2211.16605.pdf
"""
function collect_rci(search_state::SearchState{M})::Tuple{Float64,MultiRewriteConflictInfo{M}} where {M}

    rcis = Dict(
        expr.metadata.id => RewriteConflictInfo{M}(
            NaN32, false, nothing, nothing
        )
        for expr in search_state.all_nodes
    )

    for match in search_state.matches
        rcis[expr_of(match).metadata.id].rci_match_possibilities = match
    end

    # special case the identity abstraction (\x. x) since it has a self loop dependency in terms of utility calculation
    # since you can rewrite X -> (identity X) -> (identity (identity X)) -> ... as you infintely rewrite the argument
    if is_identity_abstraction(search_state)
        for expr in search_state.all_nodes
            rcis[expr.metadata.id].cumulative_utility = 0.0
            rcis[expr.metadata.id].accept_rewrite = false
        end
        return 0.0
    end

    for expr in search_state.all_nodes
        rci = rcis[expr.metadata.id]

        reject_util = sum(child -> rcis[child.metadata.id].cumulative_utility, expr.children, init=0.0)
        accept_util = if rci.rci_match_possibilities === nothing
            0.0
        else
            util, m = compute_best_utility(rcis, rci.rci_match_possibilities)
            rci.rci_match = m
            util
        end
        rci.cumulative_utility = max(reject_util, accept_util)
        rci.accept_rewrite = accept_util > reject_util + 0.0001 # slightly in favor of rejection to avoid floating point rounding errors in the approximate equality case
        rci.cumulative_utility >= 0 || error("cumulative utility should be non-negative, not $(rcis[expr.metadata.id].cumulative_utility)")

        # rci.rci_match.accept_rewrite && println("accepted rewrite at $expr with cumulative utility $(rci.rci_match.cumulative_utility) and local utility $(rci.rci_match.local_utility)")
    end

    # Eqn 18 from https://arxiv.org/pdf/2211.16605.pdf
    corpus_util = sum(programs -> minimum(p -> rcis[p.expr.metadata.id].cumulative_utility, programs), values(search_state.corpus.programs_by_task))
    util = corpus_util - search_state.abstraction.body_size
    return util, rcis
end

function compute_best_utility(rcis::MultiRewriteConflictInfo, match::MatchPossibilities)::Tuple{Float64,Match}
    (util, i) = findmax(match.alternatives) do m
        u, _ = compute_best_utility(rcis, m)
        u
    end
    return util, match.alternatives[i]
end

function compute_best_utility(rcis::MultiRewriteConflictInfo, m::Match)::Tuple{Float64,Match}
    args = vcat(m.unique_args, [v for vs in m.choice_var_captures for v in vs])
    if m.start_items !== nothing
        args = vcat(args, expr_of(m).children[1:m.start_items])
    end
    if m.end_items !== nothing
        args = vcat(args, expr_of(m).children[m.end_items+1:end])
    end
    util = m.local_utility + sum(arg -> rcis[arg.metadata.id].cumulative_utility, args, init=0.0)
    return util, m
end

function bottom_up_utility(search_state::SearchState)::Float64
    util, _ = collect_rci(search_state)
    return util
end

rewrite_program(program, search_state, rcis) = Program(rewrite_inner(program.expr, search_state, rcis), program.id, program.task)

function rewrite_inner(expr::SExpr, search_state::SearchState, rcis::MultiRewriteConflictInfo)::SExpr
    rci = rcis[expr.metadata.id]

    # if cumulative utility <= 0 then there are no rewrites in this whole subtree
    rci.cumulative_utility > 0 || return copy(expr)

    if rci.accept_rewrite
        m = rci.rci_match
        # do a rewrite
        children = [sexpr_leaf(search_state.config.new_abstraction_name)]
        for arg in m.unique_args
            push!(children, rewrite_inner(arg, search_state, rcis))
        end
        for sym in m.sym_of_idx
            push!(children, sexpr_leaf(sym))
        end
        for capture_subseq in m.choice_var_captures
            nodes = SExpr[]
            push!(nodes, sexpr_leaf(SYM_CHOICE_SEQ_HEAD))
            for capture in capture_subseq
                push!(nodes, rewrite_inner(capture, search_state, rcis))
            end
            push!(children, sexpr_node(nodes))
        end
        if !isnothing(m.continuation)
            push!(children, rewrite_inner(m.continuation, search_state, rcis))
        end
        if expr_of(m).metadata.id == expr.metadata.id && (m.start_items !== nothing || m.end_items !== nothing)
            sequence = SExpr[sexpr_leaf(SYM_SEQ_HEAD)]
            if m.start_items !== nothing
                for i in 2:m.start_items
                    push!(sequence, rewrite_inner(expr.children[i], search_state, rcis))
                end
            end
            push!(sequence, sexpr_node([sexpr_leaf(SYM_SPLICE), sexpr_node(children)]))
            if m.end_items !== nothing
                for i in m.end_items+1:length(expr.children)
                    push!(sequence, rewrite_inner(expr.children[i], search_state, rcis))
                end
            end
            out = sexpr_node(sequence)
            return out
        else
            return sexpr_node(children)
        end
    else
        # don't rewrite - just recurse
        return sexpr_node([rewrite_inner(child, search_state, rcis) for child in expr.children])
    end
end

