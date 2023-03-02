function rewrite(search_state::SearchState) :: Corpus
    (rewritten, matches) = deepcopy((search_state.corpus, search_state.matches))

    match_lookup = Dict{Int,Match}()
    for match in matches
        match_lookup[struct_hash(match.expr)] = match
    end

    for program in rewritten.programs
        rewrite_expr!(program.expr, match_lookup, search_state.new_abstraction_name)
    end

    rewritten
end

function rewrite_expr!(expr, match_lookup, new_abstraction_name)

    if !haskey(match_lookup, struct_hash(expr))
        # this was not a match location
        for arg in expr.args
            rewrite_expr!(arg, match_lookup, new_abstraction_name)
        end
        return expr
    end

    # this is a match location - lets greedily decide to rewrite
    match = match_lookup[struct_hash(expr)]
    e = curried_application(new_abstraction_name, [rewrite_expr!(detach_deepcopy(arg), match_lookup, new_abstraction_name) for arg in match.args])

    # this is a bit gross, should be a helper fn to do stuff like this
    expr.head = e.head
    expr.args = e.args
    for arg in expr.args
        arg.parent = expr
    end
    expr.struct_hash = nothing

    expr
end

