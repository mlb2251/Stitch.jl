function rewrite(search_state::SearchState) :: Corpus
    rewritten = deepcopy(search_state.corpus)

    match_lookup = Dict{Int,Match}()
    for match in search_state.matches
        match_lookup[struct_hash(match.expr)] = match
    end

    for program in rewritten.programs
        program.expr = rewrite_expr(program.expr, search_state, match_lookup)
    end

    rewritten
end

function rewrite_expr!(expr, search_state, match_lookup)

    if !haskey(match_lookup, struct_hash(expr))
        # this was not a match location
        for arg in expr.args
            rewrite_expr!(arg, search_state, match_lookup)
        end
        return
    end

    # this is a match location - lets greedily decide to rewrite
    match = match_lookup[struct_hash(expr)]
    e = curried_application(search_state.new_abstraction_name, [rewrite_expr!(detach_deepcopy(arg), search_state, match_lookup) for arg in match.args])

    # this is a bit gross, should be a helper fn to do stuff like this
    expr.head = e.head
    expr.args = e.args
    for arg in expr.args
        arg.parent = expr
    end
    expr.struct_hash = nothing

    return
end

