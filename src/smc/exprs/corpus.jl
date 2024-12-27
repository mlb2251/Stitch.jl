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
Base.isequal(p1::Production, p2::Production) = p1.type == p2.type && p1.argc == p2.argc && p1.head == p2.head
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
    size::Int
    scratch::Any
end

struct Program
    info::ProgramInfo
    expr::CorpusNode
end

struct Corpus
    programs::Vector{Program}
    bottom_up_order::Vector{CorpusNode}
end

size(node::CorpusNode) = node.size
size(corpus::Program) = size(corpus.expr)
size(corpus::Corpus) = sum(size(program) for program in corpus.programs; init=0)


function Corpus(programs::Vector{String})
    Corpus(PExpr[parse_expr(p) for p in programs], ProgramInfo.(1:length(programs)))
end

function Corpus(exprs::Vector{PExpr}, program_infos::Vector{ProgramInfo})
    roots = [make_corpus_nodes(expr, program_info.id, nothing) for (expr, program_info) in zip(exprs, program_infos)]
    programs = [Program(program_info, root) for (program_info, root) in zip(program_infos, roots)]
    return Corpus(programs)
end

function Corpus(programs::Vector{Program})
    Corpus(programs, bottom_up_order([p.expr for p in programs]))
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

    node = CorpusNode(expr, expr_id, CorpusNode[], parent, prod, prod_id, program, 1, nothing)
    if expr isa App
        # we dont do `f` - matching at `f` is not eta long
        for arg in expr.args
            arg_node = make_corpus_nodes(arg, program, node)
            node.size += size(arg_node)
            push!(node.children, arg_node)
        end
    end
    return node
end

"""
Set the scratch field of each node in the corpus. Nodes are visited in bottom-up order.
"""
function set_scratches!(f::F, corpus::Corpus) where F <: Function
    for node in corpus.bottom_up_order
        node.scratch = f(node)
    end
    nothing
end

function clear_scratches!(corpus::Corpus)
    set_scratches!(corpus, (_) -> nothing)
end

@inline function get_scratch(::Type{T}, node::CorpusNode)::T where T
    node.scratch
end

function bottom_up_order(roots::Vector{CorpusNode})
    nodes = CorpusNode[]
    worklist = copy(roots)
    while !isempty(worklist)
        node = pop!(worklist)
        push!(nodes, node)
        append!(worklist, node.children)
    end
    return reverse(nodes)
end

function getchild(node::CorpusNode, path::Path)::CorpusNode
    for i in path
        node = node.children[i]
    end
    node
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
