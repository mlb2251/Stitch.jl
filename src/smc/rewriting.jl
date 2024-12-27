

function rewritten_size(corpus::Corpus, abs::Abstraction)
    mark_rewritable_ancestors!(abs, corpus)
    size = 0
    for node in corpus.bottom_up_order
        scratch = node.scratch::RewriteData
        size += scratch.rewritten_size
    end
    size
end

function rewrite(corpus::Corpus, abs::Abstraction)
    rewritten = Program[]
    mark_rewritable_ancestors!(abs, corpus)
    for program in corpus.programs
        rw = rewrite(program.expr, abs)
        push!(rewritten, Program(program.info, make_corpus_nodes(rw, program.info.id, nothing)))
    end
    Corpus(rewritten)
end

function rewrite(node::CorpusNode, abs::Abstraction)::PExpr
    scratch = node.scratch::RewriteData
    # node is not affected by rewriting
    !scratch.is_ancestor_of_match && return node.expr

    # if node is a match with a yes-decision, rewrite it
    if scratch.is_match
        match = scratch.match::MatchDecision
        if match.decision
            args = map(match.args) do arg
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
    set_scratches!((node) -> RewriteData(false, false, size(node), nothing), corpus)

    """
    From each match location, walk up the chain of parents until we hit the root and
    mark them as an ancestor of a match (which means they can be affected by rewriting).
    """
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

    for node in corpus.bottom_up_order
        scratch = node.scratch::RewriteData
        !scratch.is_match && continue
        args = abstraction_args(node, abs)
        # size without rewriting is just based on size of children
        size_no_rewrite = 1 + sum(get_scratch(RewriteData, child).rewritten_size for child in node.children; init=0)
        # size with rewriting is based on the actual args
        size_yes_rewrite = 1 + sum(get_scratch(RewriteData, arg).rewritten_size for arg in args; init=0)
        decision = size_yes_rewrite < size_no_rewrite
        scratch.rewritten_size = min(size_no_rewrite, size_yes_rewrite)
        scratch.match = MatchDecision(size_no_rewrite, size_yes_rewrite, args, decision)
    end

    nothing
end

mutable struct MatchDecision
    size_no_rewrite::Int
    size_yes_rewrite::Int
    args::Vector{CorpusNode}
    decision::Bool
end

mutable struct RewriteData
    is_match::Bool
    is_ancestor_of_match::Bool
    rewritten_size::Int
    match::Union{MatchDecision, Nothing}
end

size(data::RewriteData) = data.match.size_best

"""
Get the CorpusNodes that are the arguments you would use if this node was being
rewritten to use the abstraction.
"""
function abstraction_args(node::CorpusNode, abs::Abstraction)
    # we need to reverse the metavar paths because metavar_paths[1] (after filtering
    # for representative paths) is meant to be de bruijn index $1 which is the innermost lambda and thus
    # the outermost (final) application argument
    map(reverse(filter(p -> p.representative, abs.metavar_paths))) do argpath
        getchild(node, argpath)
    end
end
