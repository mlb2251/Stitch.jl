
"""
A path to a metavar in a corpus node.

- `path` is the path from the match location to the corpus node corresponding to this metavar
- `subpaths` todo
"""
struct MetaVarPath
    paths::Vector{Path}
end
Base.copy(m::MetaVarPath) = MetaVarPath(Path[copy(path) for path in m.paths])
Base.hash(m::MetaVarPath, h::UInt) = hash(m.paths, h)
Base.isequal(m1::MetaVarPath, m2::MetaVarPath) = m1.paths == m2.paths

@inline primary_path(m::MetaVarPath) = @inbounds m.paths[1]
@inline has_multiuses(m::MetaVarPath) = length(m.paths) > 1
function set_indices!(node::PExpr, m::MetaVarPath, idx::Int)
    for path in m.paths
        setchild!(node, path, MetaVar(idx))
    end
end

function insert_sorted!(m::MetaVarPath, path::Path)
    i = searchsortedfirst(m.paths, path)
    insert!(m.paths, i, path)
    nothing
end

function insert_sorted!(m::MetaVarPath, paths::Vector{Path})
    for path in paths
        insert_sorted!(m, path)
    end
    nothing
end


@inline getchild(node::CorpusNode, path::MetaVarPath)::CorpusNode = getchild(node, primary_path(path))
@inline getchild(node::PExpr, path::MetaVarPath)::PExpr = getchild(node, primary_path(path))
@inline setchild!(node::PExpr, path::MetaVarPath, child::PExpr) = setchild!(node, primary_path(path), child)


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
    print(io, "[matches=", length(a.matches), " arity=", arity(a), " utility=", a.utility, ": ")
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

function assert_valid(abs::Abstraction)
    for (i, path) in enumerate(abs.metavar_paths)
        @assert getchild(abs.expr, path).name == i
    end
end

arity(abs::Abstraction) = length(abs.metavar_paths)
multiuses(abs::Abstraction) = sum(length(p.paths) - 1 for p in abs.metavar_paths; init=0)

function identity_abstraction(corpus, name::Symbol)
    # return Abstraction([Match(node, CorpusNode[node]) for node in nodes], Path[Int[]], MetaVar(1, metavar_names[1]), 2, 0, 0.)
    return Abstraction(copy(corpus.bottom_up_order), MetaVarPath[MetaVarPath(Path[Path()])], MetaVar(1), 0, 0., name, corpus)
end
