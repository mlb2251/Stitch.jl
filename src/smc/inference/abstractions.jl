
"""
A path to a metavar in a corpus node.

- `path` is the path from the match location to the corpus node corresponding to this metavar
- `subpaths` todo
"""
mutable struct MetaVarPath
    path::Path
    subpaths::Vector{Path}
end
Base.copy(m::MetaVarPath) = MetaVarPath(copy(m.path), Path[copy(subpath) for subpath in m.subpaths])

@inline isfrozen(m::MetaVarPath) = !isempty(m.subpaths)

@inline getchild(node::CorpusNode, path::MetaVarPath)::CorpusNode = getchild(node, path.path)
@inline getchild(node::PExpr, path::MetaVarPath)::PExpr = getchild(node, path.path)
@inline setchild!(node::PExpr, path::MetaVarPath, child::PExpr) = setchild!(node, path.path, child)


mutable struct Abstraction
    matches::Vector{CorpusNode}
    metavar_paths::Vector{MetaVarPath}
    expr::PExpr
    size::Int
    utility::Float64
    name::Symbol
    corpus::Corpus
end

# shallow copy matches, deep copy metavar_paths
Base.copy(a::Abstraction) = Abstraction(copy(a.matches), MetaVarPath[copy(p) for p in a.metavar_paths], copy(a.expr), a.size, a.utility, a.name, a.corpus)

function Base.show(io::IO, a::Abstraction)
    print(io, "[matches=", length(a.matches), " arity=", arity(a), " utility=", a.utility, " :")
    # if length(a.metavar_paths) > 0
    #     print(io, "(λ")
    #     for (i, _) in enumerate(a.metavar_paths)
    #         printstyled(io, metavar_names[i], " "; color=i%8)
    #     end
    #     print(io, "-> ")
    # end
    print(io, a.expr)
    # length(a.metavar_paths) > 0 && print(io, ")")
    print(io, "]")
end

arity(abs::Abstraction) = length(abs.metavar_paths)
multiuses(abs::Abstraction) = sum(length(p.subpaths) for p in abs.metavar_paths; init=0)

function identity_abstraction(corpus, name::Symbol)
    # return Abstraction([Match(node, CorpusNode[node]) for node in nodes], Path[Int[]], MetaVar(1, metavar_names[1]), 2, 0, 0.)
    return Abstraction(copy(corpus.bottom_up_order), MetaVarPath[MetaVarPath(Path(), Path[])], MetaVar(1), 0, 0., name, corpus)
end
