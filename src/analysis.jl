
function compute_score_for_search(corpus, search_config_args, start_prog)::Float64
    state, _ = stitch_search(corpus, SearchConfig(; search_config_args..., follow=true, track=start_prog))
    if state === nothing
        return 0.0
    end
    bottom_up_utility(state)
end

function find_holes(expr)
    holes = SExpr[]
    for node in subexpressions(expr)
        if is_hole(node)
            push!(holes, node)
        end
    end
    holes
end

function replace_holes_with_variables(prog, original_arity)
    prog_vars = deepcopy(prog)
    holes = find_holes(prog_vars)
    for i in original_arity:original_arity - 1 + length(holes)
        # iterate through holes backwards since that's what the code does.
        holes[original_arity + length(holes) - i].leaf = Symbol("#$i")
    end
    prog_vars, original_arity + length(holes)
end

function compute_best_score_and_lower_bound(abstr, corpus, search_config_args)

    util_real = compute_score_for_search(corpus, (; search_config_args..., silent=true), abstr.body)

    prog_vars, new_arity  = replace_holes_with_variables(abstr.body, abstr.arity)
    util_lb = compute_score_for_search(corpus, (; search_config_args..., max_arity=new_arity, follow_precisely=true, silent=true), prog_vars)
    return util_real, util_lb
end

function bounds_analysis(corpus, count)
    results = []
    items = intermediate_search_results(corpus)
    chunk = div(length(items), count)
    println("Chunk size: $chunk")
    if chunk < 1
        chunk = 1
    end
    for (i, (abstr, util_ub)) in enumerate(items)
        if i % chunk != 0
            continue
        end
        println(abstr.body)
        util_real, util_lb = compute_best_score_and_lower_bound(abstr, corpus, (;))
        push!(results, (abstr.body, util_ub, util_real, util_lb))
    end
    results
end