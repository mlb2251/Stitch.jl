
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
    name::Int
end

struct Prim <: PExpr
    name::Symbol
end

Base.copy(e::App) = App(copy(e.f), PExpr[copy(arg) for arg in e.args])
# Base.copy(e::Abs) = Abs(e.argc, copy(e.argnames), copy(e.body))
# Base.copy(e::Var) = Var(e.idx, e.name)
Base.copy(e::MetaVar) = MetaVar(e.name)
Base.copy(e::Prim) = Prim(e.name)

const eq_worklists1 = Vector{PExpr}[PExpr[] for _ in 1:Threads.nthreads()]
const eq_worklists2 = Vector{PExpr}[PExpr[] for _ in 1:Threads.nthreads()]
function Base.isequal(e1::PExpr, e2::PExpr)
    worklist1 = eq_worklists1[Threads.threadid()]
    worklist2 = eq_worklists2[Threads.threadid()]
    push!(worklist1, e1)
    push!(worklist2, e2)
    while !isempty(worklist1)
        e1 = pop!(worklist1)
        e2 = pop!(worklist2)
        if e1 isa App
            e2 isa App || return false
            length(e1.args) == length(e2.args) || return false
            push!(worklist1, e1.f)
            push!(worklist2, e2.f)
            for i in 1:length(e1.args)
                @inbounds push!(worklist1, e1.args[i])
                @inbounds push!(worklist2, e2.args[i])
            end
        elseif e1 isa MetaVar
            e2 isa MetaVar || return false
            e1.name == e2.name || return false
        elseif e1 isa Prim
            e2 isa Prim || return false
            e1.name == e2.name || return false
        else
            error("unknown node type: $e1")
        end
    end
    true
end


const hash_worklists = Vector{PExpr}[PExpr[] for _ in 1:Threads.nthreads()]
function Base.hash(e::PExpr, h::UInt)
    worklist = hash_worklists[Threads.threadid()]
    push!(worklist, e)
    while !isempty(worklist)
        node = pop!(worklist)
        if node isa App
            h = hash(:app, h)::UInt
            push!(worklist, node.f)
            append!(worklist, node.args)
        elseif node isa MetaVar
            h = hash(node.name, h)::UInt
        elseif node isa Prim
            h = hash(node.name, h)::UInt
        else
            error("unknown node type: $node")
        end
    end
    h
end



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

function getchild(node::PExpr, path::T)::PExpr where T <: AbstractVector{Int}
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
    parent = getchild(node, view(path, 1:length(path)-1))
    @assert parent isa App
    # could be a little broken around how Abs / body is handled
    @inbounds parent.args[path[end]] = child
    node
end