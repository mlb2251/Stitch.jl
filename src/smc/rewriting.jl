

function rewritten_size(corpus::Corpus, abs::Abstraction)
    bottom_up_rewrite_calculations!(abs, corpus)
    size = 0
    for program in corpus.programs
        size += program.expr.rewrite_data.rewritten_size
    end
    unmark_rewritable_ancestors!(abs)
    size
end

function rewrite(corpus::Corpus, abs::Abstraction)
    rewritten = Program[]
    bottom_up_rewrite_calculations!(abs, corpus)
    for program in corpus.programs
        rw = rewrite(program.expr, abs)
        rw_program = Program(program.info, make_corpus_nodes(rw, program.info.id, nothing))
        @assert size(rw_program) == program.expr.rewrite_data.rewritten_size "rewritten size mismatch: $(size(rw_program)) != $(program.expr.rewrite_data.rewritten_size) for program $(rw_program) and abstraction $abs"
        push!(rewritten, rw_program)
    end
    unmark_rewritable_ancestors!(abs)
    assert_cleared_rewrite_data!(corpus)
    Corpus(rewritten)
end

function rewrite(node::CorpusNode, abs::Abstraction)::PExpr
    rewrite_data = node.rewrite_data
    # node is not affected by rewriting
    !rewrite_data.is_ancestor_of_match && return node.expr

    # if node is a match with a yes-decision, rewrite it
    if rewrite_data.is_match
        if rewrite_data.decision
            # we need to reverse the metavar paths because metavar_paths[1] (after filtering
            # for representative paths) is meant to be de bruijn index $1 which is the innermost lambda and thus
            # the outermost (final) application argument
            # also it needs to be an Any[] array because of where it's used later
            args = reverse!(PExpr[rewrite(getchild(node, path), abs) for path in abs.metavar_paths])
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

function clear_rewrite_data!(corpus::Corpus)
    for node in corpus.bottom_up_order
        node.rewrite_data.is_match = false
        node.rewrite_data.is_ancestor_of_match = false
        node.rewrite_data.rewritten_size = size(node)
    end
end

function assert_cleared_rewrite_data!(corpus::Corpus)
    for node in corpus.bottom_up_order
        @assert !node.rewrite_data.is_match "is_match should be false"
        @assert !node.rewrite_data.is_ancestor_of_match "is_ancestor_of_match should be false"
        @assert node.rewrite_data.rewritten_size == size(node) "rewritten_size should be the size of the node"
    end
end

 """
From each match location, walk up the chain of parents until we hit the root and
mark them as an ancestor of a match (which means they can be affected by rewriting).
"""
function mark_rewritable_ancestors!(abs::Abstraction)
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
end

function unmark_rewritable_ancestors!(abs::Abstraction)
    for match in abs.matches
        match.rewrite_data.is_match = false
        node = match
        while true
            isnothing(node) && break
            !node.rewrite_data.is_ancestor_of_match && break
            node.rewrite_data.is_ancestor_of_match = false
            node.rewrite_data.rewritten_size = size(node)
            node = node.parent
        end
    end
end


function bottom_up_rewrite_calculations!(abs::Abstraction, corpus::Corpus)
    # set_scratches!((node) -> RewriteData(false, false, size(node), nothing), corpus)
    
    # clear_rewrite_data!(corpus)
    mark_rewritable_ancestors!(abs)

    for node in corpus.bottom_up_order
        rewrite_data = node.rewrite_data
        !rewrite_data.is_ancestor_of_match && continue

        if !rewrite_data.is_match
            # just update rewritten sizes
            rewrite_data.rewritten_size = 1 + sum(child -> child.rewrite_data.rewritten_size, node.children; init=0)
            continue
        end

        # size without rewriting is just based on size of children
        size_no_rewrite = 1 + sum(child -> child.rewrite_data.rewritten_size, node.children; init=0)
        # size with rewriting is based on the actual args
        size_yes_rewrite = 1 + sum(path -> size(getchild(node, path)), abs.metavar_paths; init=0)
        decision = size_yes_rewrite < size_no_rewrite
        rewrite_data.rewritten_size = min(size_no_rewrite, size_yes_rewrite)
        rewrite_data.decision = decision
    end

    nothing
end


