
mutable struct RewriteConflictInfo{M}
    # handling rewrite conflicts. When the same abstraction can be used in two overlapping places, we need to pick one.
    # e.g., program = (foo (foo (foo x))). abstraction = (foo (foo #0)). abstraction can either match
    # as (fn_1 (foo x)) or (foo (fn_1 x)). We need to pick one.

    # Simple bottom-up dynamic programming to figure out which is best
    cumulative_utility::Float32
    accept_rewrite::Bool
    rci_match_possibilities::Vector{M}
    rci_matches::Vector{Match}
end

const MultiRewriteConflictInfo{M} = Dict{Int64,RewriteConflictInfo{M}}

function rewrite(search_state::SearchState)::Tuple{Corpus,Float32,Float32}

    cumulative_utility, rci = collect_rci(search_state)

    rewritten_programs = [rewrite_program(program, search_state, rci) for program in search_state.corpus.programs]
    rewritten = Corpus(rewritten_programs)

    size_by_symbol = search_state.config.size_by_symbol
    corpus_compression_utility = size(search_state.corpus, size_by_symbol) - size(rewritten, size_by_symbol)
    abstraction_size_utility = -search_state.abstraction.body_size
    num_matches = sum(length(r.rci_matches) for r in values(rci))
    additional_per_match_utility = (
        # adding 1 because that's already handled in the corpus compression utility by the fn_K symbol
        1 + search_state.config.application_utility_fixed
        # per-metavariable utility
        + search_state.config.application_utility_metavar * search_state.abstraction.arity
        # per-symvar utility
        + search_state.config.application_utility_symvar * search_state.abstraction.sym_arity
        # per-choice-var utility
        + search_state.config.application_utility_choicevar * search_state.abstraction.choice_arity
    ) * num_matches
    compressive_utility = corpus_compression_utility + abstraction_size_utility + additional_per_match_utility

    # @show rewritten
    if !isapprox(cumulative_utility, compressive_utility, rtol=1e-5)
        println("rewritten programs:")
        println(rewritten)
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
            NaN32, false, M[], Match[]
        )
        for expr in search_state.all_nodes
    )

    for match in search_state.matches
        rci_expr = rcis[expr_of(match).metadata.id]
        push!(rci_expr.rci_match_possibilities, match)
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
        accept_util, ms = compute_best_utility(rcis, rci.rci_match_possibilities)
        rci.rci_matches = ms
        rci.cumulative_utility = max(reject_util, accept_util)
        rci.accept_rewrite = accept_util > reject_util + 0.0001 # slightly in favor of rejection to avoid floating point rounding errors in the approximate equality case
        # println("expr:", expr)
        # println("accept rewrite:", rci.accept_rewrite, " accept_util:", accept_util, " reject_util:", reject_util)
        rci.cumulative_utility >= 0 || error("cumulative utility should be non-negative, not $(rcis[expr.metadata.id].cumulative_utility)")

        # rci.rci_match.accept_rewrite && println("accepted rewrite at $expr with cumulative utility $(rci.rci_match.cumulative_utility) and local utility $(rci.rci_match.local_utility)")
    end

    println(rcis)

    # Eqn 18 from https://arxiv.org/pdf/2211.16605.pdf
    corpus_util = sum(programs -> minimum(p -> rcis[p.expr.metadata.id].cumulative_utility, programs), values(search_state.corpus.programs_by_task))
    println("corpus util: $corpus_util; abstraction size: $(search_state.abstraction.body_size)")
    util = corpus_util - search_state.abstraction.body_size
    return util, rcis
end

function compute_best_utility(rcis::MultiRewriteConflictInfo, matches::Vector{Match})::Tuple{Float64,Vector{Match}}
    @assert length(matches) <= 1
    if isempty(matches)
        return 0.0, []
    else
        util, m = compute_best_utility(rcis, matches[1])
        return util, [m]
    end
end

function compute_best_utility(rcis::MultiRewriteConflictInfo, matches::Vector{MatchPossibilities})::Tuple{Float64,Vector{Match}}
    if isempty(matches)
        return 0.0, []
    end
    if length(matches) == 1
        util, m = compute_best_utility(rcis, matches[1])
        return util, [m]
    end
    best_each = [compute_best_utility(rcis, m; no_start_end=true) for m in matches]
    matches = [m for (_, m) in best_each]
    utilities = [u for (u, _) in best_each]
    # println("utilities: $utilities")
    expr = best_each[1][2].expr
    @assert expr.children[1].leaf == SYM_SEQ_HEAD
    sequence_length = length(expr.children)
    pointer_back = Int64[]
    scores = Float32[]
    match_selected = [0 for _ in 1:sequence_length] 
    loc_to_match_ending_at_loc = Dict{Int32,Vector{Int32}}()
    for (i, m) in enumerate(matches)
        ei = m.end_items
        if ei !== nothing
            push!(get!(loc_to_match_ending_at_loc, ei, Int32[]), i)
        end
    end
    for i in 1:sequence_length
        push!(scores, rcis[expr.children[i].metadata.id].cumulative_utility + (if i == 1 0 else scores[i-1] end))
        push!(pointer_back, i-1)
        if haskey(loc_to_match_ending_at_loc, i)
            for match_idx in loc_to_match_ending_at_loc[i]
                m = matches[match_idx]
                start_items = m.start_items
                if start_items !== nothing
                    score = utilities[match_idx] + (if start_items == 1 0 else scores[start_items] end)
                    if score > scores[i]
                        scores[i] = score
                        pointer_back[i] = start_items
                        match_selected[i] = match_idx
                    end
                end
            end
        end
    end
    selected_matches = Match[]
    loc = sequence_length
    while loc > 0
        selected = match_selected[loc]
        if selected != 0
            push!(selected_matches, matches[selected])
        end
        loc = pointer_back[loc]
    end
    sort!(selected_matches, by=m -> m.start_items)
    # println(expr)
    # println(loc_to_match_ending_at_loc)
    # println(sequence_length)
    # println([(m.start_items, m.end_items) for m in matches])
    # println(scores)
    # println(pointer_back)
    return scores[end], selected_matches
end

function compute_best_utility(rcis::MultiRewriteConflictInfo, match::MatchPossibilities; no_start_end=false)::Tuple{Float64,Match}
    (util, i) = findmax(match.alternatives) do m
        u, _ = compute_best_utility(rcis, m; no_start_end=no_start_end)
        u
    end
    return util, match.alternatives[i]
end

function compute_best_utility(rcis::MultiRewriteConflictInfo, m::Match; no_start_end=false)::Tuple{Float64,Match}
    args = vcat(m.unique_args, [v for vs in m.choice_var_captures for v in vs])
    if !no_start_end
        if m.start_items !== nothing
            args = vcat(args, expr_of(m).children[1:m.start_items])
        end
        if m.end_items !== nothing
            args = vcat(args, expr_of(m).children[m.end_items+1:end])
        end
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
    # println("rewrite inner", expr)
    rci = rcis[expr.metadata.id]

    # if cumulative utility <= 0 then there are no rewrites in this whole subtree
    rci.cumulative_utility > 0 || return copy(expr)

    if rci.accept_rewrite
        stub_children = [match_to_stub(m, search_state, rcis) for m in rci.rci_matches]
        if length(stub_children) == 1
            m = rci.rci_matches[1]
            if m.start_items === nothing && m.end_items === nothing
                return stub_children[1]
            end
        end
        @assert length(stub_children) >= 1
        for m in rci.rci_matches
            @assert expr_of(m).metadata.id == expr.metadata.id && (m.start_items !== nothing || m.end_items !== nothing)
        end
        sequence = SExpr[sexpr_leaf(SYM_SEQ_HEAD)]
        first_match = rci.rci_matches[1]
        add_to_sequence(expr, sequence, search_state, rcis, 2:first_match.start_items)
        ends_each = [m.start_items for m in rci.rci_matches[2:end]]
        push!(ends_each, length(expr.children))
        for (m, stub, end_each) in zip(rci.rci_matches, stub_children, ends_each)
            push!(sequence, sexpr_node([sexpr_leaf(SYM_SPLICE), stub]))
            if m.end_items !== nothing
                add_to_sequence(expr, sequence, search_state, rcis, m.end_items+1:end_each)
            end
        end
        
        out = sexpr_node(sequence)
        return out
    else
        # don't rewrite - just recurse
        return sexpr_node([rewrite_inner(child, search_state, rcis) for child in expr.children])
    end
end

function match_to_stub(m::Match, search_state::SearchState, rcis::MultiRewriteConflictInfo)::SExpr
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
    return sexpr_node(children)
end

function add_to_sequence(expr::SExpr, sequence::Vector{SExpr}, search_state::SearchState, rcis::MultiRewriteConflictInfo, r::UnitRange{Int64})
    for i in r
        push!(sequence, rewrite_inner(expr.children[i], search_state, rcis))
    end
end