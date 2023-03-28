function rewrite(search_state::SearchState; no_rewrite=false, local_rewrite_util=nothing) :: Tuple{Corpus,Float32}
    (rewritten, matches) = if no_rewrite
        deepcopy((search_state.corpus, search_state.matches))
    else
        (search_state.corpus, search_state.matches)
    end

    match_lookup = Dict{Int,Match}()
    for match in matches
        match_lookup[struct_hash(match.expr)] = match
    end

    total_util = 0.0

    for program in rewritten.programs
        (_,util) = rewrite_expr!(program.expr, match_lookup, search_state.new_abstraction_name, no_rewrite)
        total += util
    end

    (rewritten,total_util)
end

function rewrite_expr!(expr, match_lookup, new_abstraction_name, no_rewrite) :: Tuple{SExpr,Float32}

    if !haskey(match_lookup, struct_hash(expr))
        # this was not a match location
        for arg in expr.args
            rewrite_expr!(arg, match_lookup, new_abstraction_name, no_rewrite)
        end
        return expr
    end

    # this is a match location - lets greedily decide to rewrite
    match = match_lookup[struct_hash(expr)]

    if no_rewrite
        util = sum(arg -> rewrite_expr!(detach_deepcopy(arg), match_lookup, new_abstraction_name, no_rewrite)[2], match.args)
        util += size(Expr(new_abstraction_name,Expr[]))
        return (expr, util)
    end


    e = curried_application(new_abstraction_name, [rewrite_expr!(detach_deepcopy(arg), match_lookup, new_abstraction_name, no_rewrite) for arg in match.args])

    # this is a bit gross, should be a helper fn to do stuff like this
    expr.head = e.head
    expr.args = e.args
    for arg in expr.args
        arg.parent = expr
    end
    expr.struct_hash = nothing

    expr
end



function rewrite_one(expr; with_rewrite=true)

end




