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

"""
There is one CorpusNode for each grammar production it would take to build the expression.
"""
mutable struct CorpusNode
    expr::PExpr
    expr_id::Int
    children::Vector{CorpusNode}
    parent::Union{CorpusNode, Nothing}
    production::Production
    production_id::Int
    program::Int
    scratch::Any
end

struct Program
    info::ProgramInfo
    expr::CorpusNode
end

struct Corpus
    programs::Vector{Program}
end

size(node::CorpusNode) = 1 + sum(size(child) for child in node.children; init=0)
size(corpus::Program) = size(corpus.expr)
size(corpus::Corpus) = sum(size(program) for program in corpus.programs; init=0)


function Corpus(programs::Vector{String})
    Corpus(PExpr[parse_expr(p) for p in programs])
end

function Corpus(exprs::Vector{PExpr})
    program_infos = ProgramInfo.(1:length(exprs))
    nodes = [make_corpus_nodes(expr, program_info.id, nothing) for (expr, program_info) in zip(exprs, program_infos)]
    return Corpus([Program(program_info, node) for (program_info, node) in zip(program_infos, nodes)])
end

function load_corpus(path::String)
    programs = String.(JSON.parsefile(path))
    Corpus(programs)
end

function Base.show(io::IO, c::Corpus)
    for (i,p) in enumerate(c.programs)
        print(io, p.info.id, ": ", p.expr)
        i < length(c.programs) && println(io)
    end
end

function Base.show(io::IO, n::CorpusNode)
    print(io, n.expr)
end

function make_corpus_nodes(expr::PExpr, program::Int, parent::Union{Nothing, CorpusNode})
    # if expr isa Abs
    #     # we skip over lambdas because you cant match at them
    #     return CorpusNode(expr.body, program)
    # end

    expr_id = expr_idset[expr]
    prod = Production(expr)
    prod_id = production_idset[prod]

    node = CorpusNode(expr, expr_id, CorpusNode[], parent, prod, prod_id, program, nothing)
    if expr isa App
        # we dont do `f` - matching at `f` is not eta long
        for arg in expr.args
            push!(node.children, make_corpus_nodes(arg, program, node))
        end
    end
    return node
end

function set_scratches!(f::F, corpus::Corpus) where F <: Function
    worklist = CorpusNode[program.expr for program in corpus.programs]
    while !isempty(worklist)
        node = pop!(worklist)
        node.scratch = f(node)
        append!(worklist, node.children)
    end
    nothing
end

function clear_scratches!(corpus::Corpus)
    set_scratches!(corpus, (_) -> nothing)
end

@inline function get_scratch(::Type{T}, node::CorpusNode)::T where T
    node.scratch
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
    for program in corpus.programs
        descendants(program.expr; nodes=nodes)
    end
    return nodes
end

function getchild(node::CorpusNode, path::Path)::CorpusNode
    for i in path
        node = node.children[i]
    end
    return node
end

function has_prim(corpus::Corpus, prim::Symbol)
    any(program -> has_prim(program.expr, prim), corpus.programs)
end

function has_prim(node::CorpusNode, prim::Symbol)
    expr = node.expr
    if expr isa Prim
        return expr.name === prim
    elseif expr isa App
        return any(child -> has_prim(child, prim), node.children)
    elseif expr isa MetaVar
        error("MetaVar can't exist in corpus")
    else
        error("not implemented: $(typeof(expr))")
    end
end
