
"""
A path to a metavar in a corpus node.

- `path` is the path from the match location to the corpus node corresponding to this metavar
- `name` is the unique name of the metavar - two metavars with the same name are the same metavar. This is NOT a de bruijn index
  but each unique name will ultimately map to a unique de bruijn index. 
- `frozen` indicates that the metavar can no longer be expanded because it was used in a multiuse expansion
  so changing its index would invalidate the match location subsetting done by the multiuse
- `representative` indicates that this path is the one that should be used to represent the metavar
  among all the paths that have the same idx. The order of the representative paths in the abstraction gives the de bruijn index order.
"""
mutable struct MetaVarPath
    path::Path
    name::Int
    frozen::Bool
    representative::Bool
end
Base.copy(m::MetaVarPath) = MetaVarPath(copy(m.path), m.name, m.frozen, m.representative)

getchild(node::CorpusNode, path::MetaVarPath)::CorpusNode = getchild(node, path.path)
getchild(node::PExpr, path::MetaVarPath)::PExpr = getchild(node, path.path)
setchild!(node::PExpr, path::MetaVarPath, child::PExpr) = setchild!(node, path.path, child)



mutable struct Abstraction
    # matches::Vector{Match}
    matches::Vector{CorpusNode}
    metavar_paths::Vector{MetaVarPath}
    expr::PExpr
    fresh_metavar::Int
    size::Int
    multiuses::Int
    utility::Float64
    arity::Int
    name::Symbol
end

# Base.copy(a::Abstraction) = Abstraction(Match[copy(m) for m in a.matches], copy.(a.metavar_paths), copy(a.expr), a.fresh_metavar, a.size, a.utility)
Base.copy(a::Abstraction) = Abstraction(copy(a.matches), MetaVarPath[copy(p) for p in a.metavar_paths], copy(a.expr), a.fresh_metavar, a.size, a.multiuses, a.utility, a.arity, a.name)

function Base.show(io::IO, a::Abstraction)
    print(io, "[matches=", length(a.matches), " arity=", a.arity, " utility=", a.utility, " :")
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

function identity_abstraction(corpus, name::Symbol)
    nodes = descendants(corpus)
    # return Abstraction([Match(node, CorpusNode[node]) for node in nodes], Path[Int[]], MetaVar(1, metavar_names[1]), 2, 0, 0.)
    metavar_idx = 1
    return Abstraction(nodes, MetaVarPath[MetaVarPath(Path(), metavar_idx, false, true)], MetaVar(metavar_idx), metavar_idx+1, 0, 0, 0., 1, name)
end
