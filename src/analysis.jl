
function compute_score(corpus, search_config_args, start_prog)
    state, _ = stitch_search(corpus, SearchConfig(; search_config_args..., follow=true, track=start_prog))
    bottom_up_utility(state)
end

function compute_scores(abstr, corpus, search_config_args)
    prog = abstr.body
    stateFinal, _ = stitch_search(corpus, SearchConfig(; search_config_args..., follow=true, track=prog))

    println(stateFinal)

    println("COMPUTING UTILITY")
    util_real = bottom_up_utility(stateFinal)
    println("COMPUTING UTILITY", util_real)

    return 0, 0

    function replace_holes_with_variables(prog, original_arity)
        prog_vars = deepcopy(prog)
        holes = find_holes(prog_vars)
        for i in original_arity:original_arity - 1 + length(holes)
            # iterate through holes backwards since that's what the code does.
            holes[original_arity + length(holes) - i].leaf = Symbol("#$i")
        end
        prog_vars, original_arity + length(holes)
    end

    prog_vars, new_arity  = replace_holes_with_variables(prog, abstr.arity)

    state_vars, _ = stitch_search(corpus, SearchConfig(; search_config_args..., follow=true, track=prog_vars, max_arity=new_arity, upper_bound_fn=(state, matches) -> Inf32))
    println("COMPUTING UTILITY")
    util_lb = bottom_up_utility(state_vars)
    println("COMPUTING UTILITY", util_lb)
    return util_real, util_lb
end