struct Production
    type::Symbol
    head::PExpr
    argc::Int
end
Production(e::App) = Production(:app, e.f, length(e.args))
# Production(e::Abs) = error("there is no production for an abstraction")
# Production(e::Var) = Production(e, 0)
Production(e::MetaVar) = error("there is no production for a metavar")
Production(e::Prim) = Production(:prim, e, 0)

Base.copy(p::Production) = Production(copy(p.type), copy(p.head), p.argc)
Base.:(==)(p1::Production, p2::Production) = p1.type == p2.type && p1.argc == p2.argc && p1.head == p2.head
Base.hash(p::Production, h::UInt) = hash(hash(p.type, hash(p.head, hash(p.argc, h))))

struct ProgramInfo
    id::Int
end

const production_idset = IdSet{Production}()
const expr_idset = IdSet{PExpr}()

mutable struct CorpusNode
    expr::PExpr
    expr_id::Int
    children::Vector{CorpusNode}
    # children_expr_paths::Vector{Path}
    production::Production
    production_id::Int
    program::ProgramInfo
end

struct Corpus
    programs::Vector{ProgramInfo}
    roots::Vector{CorpusNode}
end

function Corpus(programs::Vector{String})
    Corpus(PExpr[parse_expr(p) for p in programs])
end

function Corpus(exprs::Vector{PExpr})
    programs = ProgramInfo.(1:length(exprs))
    nodes = [make_corpus_nodes(expr, program) for (expr, program) in zip(exprs, programs)]
    return Corpus(programs, nodes)
end

function Base.show(io::IO, c::Corpus)
    for (i,(p,root)) in enumerate(zip(c.programs, c.roots))
        print(io, p.id, ": ", root)
        i < length(c.programs) && println(io)
    end
end

function Base.show(io::IO, n::CorpusNode)
    print(io, n.expr)
end

function make_corpus_nodes(expr::PExpr, program::ProgramInfo)
    # if expr isa Abs
    #     # we skip over lambdas because you cant match at them
    #     return CorpusNode(expr.body, program)
    # end

    expr_id = expr_idset[expr]
    prod = Production(expr)
    prod_id = production_idset[prod]

    node = CorpusNode(expr, expr_id, CorpusNode[], prod, prod_id, program)
    if expr isa App
        # we dont do `f` - matching at `f` is not eta long
        for arg in expr.args
            push!(node.children, make_corpus_nodes(arg, program))
        end
    end
    return node
end

function descendants(node::CorpusNode; nodes=Vector{CorpusNode}())
    for child in node.children
        push!(nodes, child)
        descendants(child; nodes=nodes)
    end
    return nodes
end

function descendants(corpus::Corpus)
    nodes = Vector{CorpusNode}()
    for root in corpus.roots
        descendants(root; nodes=nodes)
    end
    return nodes
end

function getchild(node::CorpusNode, path::Path)::CorpusNode
    for i in path
        node = node.children[i]
    end
    return node
end