
const Path = Vector{Int}

abstract type PExpr end

struct App <: PExpr
    f::PExpr
    args::Vector{PExpr}
end

# struct Abs <: PExpr
#     argc::Int
#     argnames::Vector{Symbol}
#     body::PExpr
# end

# struct Var <: PExpr
#     idx::Int
#     name::Symbol
# end

struct MetaVar <: PExpr
    idx::Int
end

struct Prim <: PExpr
    name::Symbol
end

Base.copy(e::App) = App(copy(e.f), PExpr[copy(arg) for arg in e.args])
# Base.copy(e::Abs) = Abs(e.argc, copy(e.argnames), copy(e.body))
# Base.copy(e::Var) = Var(e.idx, e.name)
Base.copy(e::MetaVar) = MetaVar(e.idx)
Base.copy(e::Prim) = Prim(e.name)

Base.:(==)(e1::App, e2::App) = e1.f == e2.f && e1.args == e2.args
# Base.:(==)(e1::Abs, e2::Abs) = e1.argc == e2.argc && e1.argnames == e2.argnames && e1.body == e2.body
# Base.:(==)(e1::Var, e2::Var) = e1.idx == e2.idx && e1.name == e2.name
Base.:(==)(e1::MetaVar, e2::MetaVar) = e1.idx == e2.idx
Base.:(==)(e1::Prim, e2::Prim) = e1.name == e2.name

Base.hash(e::App, h::UInt) = hash(e.f, hash(e.args, h))
# Base.hash(e::Abs, h::UInt) = hash(hash(e.argc, hash(e.argnames, hash(e.body, h)))
# Base.hash(e::Var, h::UInt) = hash(hash(e.idx, hash(e.name, h)))
Base.hash(e::MetaVar, h::UInt) = hash(e.idx, h)
Base.hash(e::Prim, h::UInt) = hash(e.name, h)

const NONAME::Symbol = :noname

function generate_metavar_names()
    # A..Z then AA..ZZ
    names = Symbol.(collect('A':'Z'))
    for i in 1:26
        for j in 1:26
            push!(names, Symbol(string(Char(i+64), Char(j+64))))
        end
    end
    return names
end

const metavar_names = generate_metavar_names()

function getchild(node::PExpr, path::Path)::PExpr
    # while node isa Abs
    #     node = node.body  # abs are skipped over in getchild
    # end
    for i in path
        @assert node isa App
        node = node.args[i]
        # while node isa Abs
        #     node = node.body # abs are skipped over in getchild
        # end
    end
    return node
end

function setchild!(node::PExpr, path::Path, child::PExpr)
    isempty(path) && return child
    parent = getchild(node, path[1:end-1])
    @assert parent isa App
    # could be a little broken around how Abs / body is handled
    parent.args[path[end]] = child
    node
end