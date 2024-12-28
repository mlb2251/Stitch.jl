

function rewritten_size(corpus::Corpus, abs::Abstraction)
    mark_rewritable_ancestors!(abs, corpus)
    size = 0
    for node in corpus.bottom_up_order
        size += node.rewrite_data.rewritten_size
    end
    size
end

function rewrite(corpus::Corpus, abs::Abstraction)
    rewritten = Program[]
    mark_rewritable_ancestors!(abs, corpus)
    for program in corpus.programs
        rw = rewrite(program.expr, abs)
        rw_program = Program(program.info, make_corpus_nodes(rw, program.info.id, nothing))
        @assert size(rw_program) == program.expr.rewrite_data.rewritten_size "rewritten size mismatch: $(size(rw_program)) != $(program.expr.rewrite_data.rewritten_size) for program $(rw_program) and abstraction $abs"
        push!(rewritten, rw_program)
    end
    Corpus(rewritten)
end

function rewrite(node::CorpusNode, abs::Abstraction)::PExpr
    # node is not affected by rewriting
    !node.rewrite_data.is_ancestor_of_match && return node.expr

    # if node is a match with a yes-decision, rewrite it
    if node.rewrite_data.is_match
        match = node.rewrite_data.match
        if match.decision
            args = map(match.args) do arg::CorpusNode
                rewrite(arg, abs)
            end
            return App(Prim(abs.name), args)
        end
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


function mark_rewritable_ancestors!(abs::Abstraction, corpus::Corpus)
    # set_scratches!((node) -> RewriteData(false, false, size(node), nothing), corpus)
    for node in corpus.bottom_up_order
        node.rewrite_data.is_match = false
        node.rewrite_data.is_ancestor_of_match = false
        node.rewrite_data.rewritten_size = size(node)
    end

    """
    From each match location, walk up the chain of parents until we hit the root and
    mark them as an ancestor of a match (which means they can be affected by rewriting).
    """
    for match in abs.matches
        match.rewrite_data.is_match = true
        node = match
        while true
            isnothing(node) && break
            node.rewrite_data.is_ancestor_of_match && break
            node.rewrite_data.is_ancestor_of_match = true
            node = node.parent
        end
    end

    for node in corpus.bottom_up_order
        !node.rewrite_data.is_ancestor_of_match && continue

        if !node.rewrite_data.is_match
            # just update rewritten sizes
            node.rewrite_data.rewritten_size = 1 + sum(child.rewrite_data.rewritten_size for child in node.children; init=0)
            continue
        end

        # match case
        args = abstraction_args(node, abs)
        # size without rewriting is just based on size of children
        size_no_rewrite = 1 + sum(child.rewrite_data.rewritten_size for child in node.children; init=0)
        # size with rewriting is based on the actual args
        size_yes_rewrite = 1 + sum(arg.rewrite_data.rewritten_size for arg in args; init=0)
        decision = size_yes_rewrite < size_no_rewrite
        node.rewrite_data.rewritten_size = min(size_no_rewrite, size_yes_rewrite)
        node.rewrite_data.match = MatchDecision(size_no_rewrite, size_yes_rewrite, args, decision)
    end

    nothing
end

"""
Get the CorpusNodes that are the arguments you would use if this node was being
rewritten to use the abstraction.
"""
function abstraction_args(node::CorpusNode, abs::Abstraction)
    # we need to reverse the metavar paths because metavar_paths[1] (after filtering
    # for representative paths) is meant to be de bruijn index $1 which is the innermost lambda and thus
    # the outermost (final) application argument
    map(reverse(abs.metavar_paths)) do argpath
        getchild(node, argpath)
    end
end
