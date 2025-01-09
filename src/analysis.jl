
function compute_score_for_search(corpus, search_config_args, start_prog)
    state, _ = stitch_search(corpus, SearchConfig(; search_config_args..., follow=true, track=start_prog))
    bottom_up_utility(state)
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

    util_real = compute_score_for_search(corpus, search_config_args, abstr.body)

    prog_vars, new_arity  = replace_holes_with_variables(abstr.body, abstr.arity)
    util_lb = compute_score_for_search(corpus, (; search_config_args..., max_arity=new_arity, upper_bound_fn=(state, matches) -> Inf32, follow_precisely=true, silent=true), prog_vars)
    return util_real, util_lb
end