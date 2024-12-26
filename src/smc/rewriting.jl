


function rewrite(corpus::Corpus, abs::Abstraction)::Vector{PExpr}
    rewritten_programs = PExpr[]
    mark_rewritable_ancestors!(abs, corpus)
    for i in 1:length(corpus.roots)
        rewritten_program = rewrite(corpus.roots[i], abs)
        push!(rewritten_programs, rewritten_program)
    end
    rewritten_programs
end

function rewrite(node::CorpusNode, abs::Abstraction)::PExpr
    scratch = node.scratch::RewriteData
    # node is not affected by rewriting
    !scratch.is_ancestor_of_match && return node.expr

    # TODO – for now we are greedily doing the rewrite if there's a match
    if scratch.is_match
        args = PExpr[]
        # we need to reverse the metavar paths because metavar_paths[1] is
        # meant to be de bruijn index $1 which is the innermost lambda and thus
        # the outermost (final) application argument
        for argpath in reverse(abs.metavar_paths)
            child = getchild(node, argpath)
            rewritten_child = rewrite(child, abs)
            push!(args, rewritten_child)
        end
        return App(Prim(abs.name), args)
    end

    # node is not a match but it is an ancestor of a match
    expr = node.expr
    if expr isa App
        # we dont rewrite at `f` - matching at `f` is not eta long
        return App(expr.f, PExpr[rewrite(arg, abs) for arg in node.children])
    elseif expr isa Prim
        return expr
    elseif expr isa MetaVar
        error("MetaVar should not be rewritten")
    else
        error("not implemented: $(typeof(expr))")
    end

    error("unreachable")
end

"""
From each match location, walk up the chain of parents until we hit the root and
mark them as an ancestor of a match (which means they can be affected by rewriting).
"""
function mark_rewritable_ancestors!(abs::Abstraction, corpus::Corpus)
    # affected_ancestors = IdDict{CorpusNode, Nothing}()
    set_scratches!((_) -> RewriteData(), corpus)

    for match in abs.matches
        match.scratch.is_match = true
        node = match
        while true
            isnothing(node) && break
            scratch = node.scratch::RewriteData
            scratch.is_ancestor_of_match && break
            scratch.is_ancestor_of_match = true
            node = node.parent
        end
    end
    # affected_ancestors
    nothing
end

mutable struct RewriteData
    is_match::Bool
    is_ancestor_of_match::Bool
end
RewriteData() = RewriteData(false, false)

# function rewritable_programs(abs::Abstraction)
#     Set{Int}(map(n -> n.program.id, abs.matches))
# end
