import JSON
import Random

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
    name::Symbol
end

struct Prim <: PExpr
    name::Symbol
end

Base.copy(e::App) = App(copy(e.f), PExpr[copy(arg) for arg in e.args])
# Base.copy(e::Abs) = Abs(e.argc, copy(e.argnames), copy(e.body))
# Base.copy(e::Var) = Var(e.idx, e.name)
Base.copy(e::MetaVar) = MetaVar(e.idx, e.name)
Base.copy(e::Prim) = Prim(e.name)

Base.:(==)(e1::App, e2::App) = e1.f == e2.f && e1.args == e2.args
# Base.:(==)(e1::Abs, e2::Abs) = e1.argc == e2.argc && e1.argnames == e2.argnames && e1.body == e2.body
# Base.:(==)(e1::Var, e2::Var) = e1.idx == e2.idx && e1.name == e2.name
Base.:(==)(e1::MetaVar, e2::MetaVar) = e1.idx == e2.idx && e1.name == e2.name
Base.:(==)(e1::Prim, e2::Prim) = e1.name == e2.name

Base.hash(e::App, h::UInt) = hash(hash(e.f, hash(e.args, h)))
# Base.hash(e::Abs, h::UInt) = hash(hash(e.argc, hash(e.argnames, hash(e.body, h)))
# Base.hash(e::Var, h::UInt) = hash(hash(e.idx, hash(e.name, h)))
Base.hash(e::MetaVar, h::UInt) = hash(hash(e.idx, hash(e.name, h)))
Base.hash(e::Prim, h::UInt) = hash(hash(e.name, h))


struct Production
    head::PExpr
    argc::Int
    hash::UInt
end
Production(e::App) = Production(e.f, length(e.args), hash(e.f, hash(length(e.args))))
# Production(e::Abs) = error("there is no production for an abstraction")
# Production(e::Var) = Production(e, 0)
Production(e::MetaVar) = error("there is no production for a metavar")
Production(e::Prim) = Production(e, 0, hash(e))

Base.copy(p::Production) = Production(copy(p.head), p.argc, p.hash)

Base.isequal(p1::Production, p2::Production) = p1.hash == p2.hash && p1.argc == p2.argc && p1.head == p2.head

Base.hash(p::Production, h::UInt) = hash(hash(p.head, hash(p.argc, h)))



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


struct ProgramInfo
    id::Int
end

# in future these should be aligned to things you can actually match with like production rule level
# or boop level
struct CorpusNode
    expr::PExpr
    expr_hash::UInt # NON-unique hash
    children::Vector{CorpusNode}
    # children_expr_paths::Vector{Path}
    production::Production
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

function make_corpus_nodes(expr::PExpr, program::ProgramInfo)
    # if expr isa Abs
    #     # we skip over lambdas because you cant match at them
    #     return CorpusNode(expr.body, program)
    # end

    node = CorpusNode(expr, hash(expr), CorpusNode[], Production(expr), program)
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


const Path = Vector{Int}


# struct Match
#     node::CorpusNode
#     metavar_args::Vector{CorpusNode}
# end

# Base.copy(m::Match) = Match(m.node, copy(m.metavar_args)) # can keep shallow

mutable struct Abstraction
    # matches::Vector{Match}
    matches::Vector{CorpusNode}
    metavar_paths::Vector{Path}
    expr::PExpr
    fresh_metavar::Int
    size::Int
    utility::Float64
end

# Base.copy(a::Abstraction) = Abstraction(Match[copy(m) for m in a.matches], copy.(a.metavar_paths), copy(a.expr), a.fresh_metavar, a.size, a.utility)
Base.copy(a::Abstraction) = Abstraction(copy(a.matches), Path[copy(p) for p in a.metavar_paths], copy(a.expr), a.fresh_metavar, a.size, a.utility)


function getchild(node::CorpusNode, path::Path)::CorpusNode
    for i in path
        @inbounds node = node.children[i]
    end
    return node
end

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

# function setchild!(node::CorpusNode, path::Path, child::CorpusNode)
#     for i in 1:length(path)-1
#         node = node.children[path[i]]
#     end
#     node.children[path[end]] = child
# end

function sample_expansion!(abs::Abstraction)
    length(abs.metavar_paths) == 0 && return false
    # pick a random path to expand
    i = rand(1:length(abs.metavar_paths))
    path = popat!(abs.metavar_paths, i)
    # pick a random match location to use as the basis for expansion
    match = abs.matches[rand(1:end)]

    prod = getchild(match, path).production
    # prod = match.metavar_args[i].production

    # @show abs

    # subset to the matches
    filter!(abs.matches) do node
        # if match.metavar_args[i].production == prod
        #     arg = popat!(match.metavar_args, i);
        #     for j in 1:prod.argc
        #         push!(match.metavar_args, arg.children[j])
        #     end
        #     return true
        # end
        # return false
        prod2 = getchild(node, path).production
        prod2.hash == prod.hash
    end

    if prod.argc > 0
        new_expr = App(prod.head, PExpr[])
        for j in 1:prod.argc
            push!(new_expr.args, MetaVar(abs.fresh_metavar, metavar_names[abs.fresh_metavar]))
            abs.fresh_metavar += 1
            new_path = copy(path)
            push!(new_path, j)
            push!(abs.metavar_paths, new_path)
        end
    else
        new_expr = prod.head
    end
    abs.expr = setchild!(abs.expr, path, new_expr)
    abs.size += 1
    abs.utility = length(abs.matches)*abs.size
    return true
end


function identity_abstraction(corpus)
    nodes = descendants(corpus)
    # return Abstraction([Match(node, CorpusNode[node]) for node in nodes], Path[Int[]], MetaVar(1, metavar_names[1]), 2, 0, 0.)
    return Abstraction(nodes, Path[Int[]], MetaVar(1, metavar_names[1]), 2, 0, 0.)
end

function Base.show(io::IO, a::Abstraction)
    print(io, "[matches=", length(a.matches), " arity=", length(a.metavar_paths), " utility=", a.utility, " :")
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


mutable struct Particle
    abs::Abstraction
    weight::Float64
    done::Bool
end
Base.copy(p::Particle) = Particle(copy(p.abs), p.weight, p.done)

function test(; path="data/cogsci/nuts-bolts.json", seed=nothing)
    programs = String.(JSON.parsefile(path))
    corpus = Corpus(programs)
    num_particles = 3000

    init_abs = identity_abstraction(corpus)
    init_particle = Particle(init_abs, 0., false)
    particles = Particle[copy(init_particle) for _ in 1:num_particles]

    # tmp_particles = Particle[copy(init_particle) for _ in 1:num_particles]


    best_utility = 0.
    temperature = .5

    !isnothing(seed) && Random.seed!(seed)
    # @show Random.seed!()

    while any(p -> !p.done, particles)
        for particle in particles
            # println("expand")
            res = sample_expansion!(particle.abs)
            particle.done = !res
            if particle.abs.utility > best_utility
                best_utility = particle.abs.utility
                println("new best: ", particle.abs)
            end
        end

        # println("resample")

        # resample
        for particle in particles
            particle.weight = length(particle.abs.matches)
            if particle.done
                particle.weight = 0.
            end
        end
        # weights = exp.([p.logweight/temperature for p in particles])
        weights = [exp(log(p.weight)/temperature) for p in particles]
        if sum(weights) ≈ 0
            break
        end
        weights ./= sum(weights)

        # new_idxs = Int[sample_normalized(weights) for _ in 1:num_particles]

        # for (i, idx) in enumerate(new_idxs)
        # #     @inbounds copy!(tmp_particles[i], particles[idx])
        #     particles[i] = copy(particles[idx])
        # end
        # particles, tmp_particles = tmp_particles, particles

        @inbounds particles = [copy(particles[sample_normalized(weights)]) for _ in 1:num_particles]
    end

end


"""
adapted from StatsBase.sample();
this verison requires normalized weights (uncomment the first line sum to work with unnormalized weights)
"""
function sample_normalized(weights)
    t = rand() # * sum(w -> w, weights) 
    n = length(weights)
    i = 1
    @inbounds cw = weights[1]
    while cw < t && i < n
        i += 1
        @inbounds cw += weights[i]
    end
    return i
end


Base.parse(::Type{PExpr}, s) = parse_expr(s)

function parse_expr(s)
    s = replace(
        s,
        r"\(" => " ( ",
        r"\)" => " ) ",
        "{" => " { ",
        "}" => " } ",
        "->" => " -> ",
        "λ" => " λ ",
        "," => " , ",
        r"\s+" => " " # must come at end – collapse extra whitespace
    )
    tokens = split(s)
    expr, rest = parse_expr_inner(tokens, Symbol[])
    @assert isempty(rest) "parse error: trailing characters: $rest"
    return expr
end

function parse_expr_inner(tokens, env::Vector{Symbol})
    length(tokens) == 0 && error("unexpected end of input")
    token = tokens[1]
    token_sym = Symbol(token)
    if token == "("
        # Possible expression heads
        tokens = tokens[2:end]
        token = tokens[1]
        if token == "lam" || token == "lambda" || token == "λ"
            # parse (λx y z -> body) or (λx,y,z -> body) or (λ_ _ _ -> body) or (λ_ -> body)
            # semantically equivalent to (λx -> (λy -> (λz -> body)))
            tokens = tokens[2:end]
            num_args = 0
            argnames = Symbol[]
            while true
                name = tokens[1]
                @assert Base.isidentifier(name) "expected identifier for lambda argument, got $name"
                env = [name, env...]
                push!(argnames, Symbol(name))
                num_args += 1
                tokens = tokens[2:end]
                if tokens[1] == "," # optional comma
                    tokens = tokens[2:end]
                end
                if tokens[1] == "->" # end of arg list
                    tokens = tokens[2:end]
                    break
                end
            end
            body, tokens = parse_expr_inner(tokens, env)
            tokens[1] != ")" && error("expected closing paren")
            @assert false
            # return Abs(num_args, argnames, body), tokens[2:end]
        else
            # Parse an application
            f, tokens = parse_expr_inner(tokens, env)
            args = []
            while tokens[1] != ")"
                arg, tokens = parse_expr_inner(tokens, env)
                push!(args, arg)
            end
            return App(f, args), tokens[2:end]
        end
    elseif token_sym ∈ env
        # Parse a var by name like "foo"
        idx = findfirst(x -> x === token_sym, env) # shadowing
        @assert false
        # return Var(idx, token_sym), tokens[2:end]
    elseif token[1] == '#'
        # parse debruijn index #4
        idx = parse(Int, token[2:end])
        @assert idx > 0 "need one-index debruijn"
        @assert false
        # return Var(idx, get!(env, idx, NONAME)), tokens[2:end]
    elseif '#' ∈ token
        # variable combining name and debruijn like x#4
        parts = split(token, "#")
        name = Symbol(parts[1])
        idx = parse(Int, parts[2])
        @assert parts[1] == env[idx] "debruijn index must match variable name"
        @assert false
        # return Var(idx, name), tokens[2:end]
    else
        return Prim(token_sym), tokens[2:end]
    end
end

function Base.show(io::IO, e::App)
    paren_depth = get(io, :paren_depth, 0)::Int
    # printstyled(io, "("; color=paren_depth%8)
    print(io, "(")
    new_io = IOContext(io, :paren_depth => paren_depth + 1)
    print(new_io, e.f)
    for arg in e.args
        print(new_io, " ", arg)
    end
    print(io, ")")
    # printstyled(io, ")"; color=paren_depth%8)
end
# function Base.show(io::IO, e::Abs)
#     print(io, "(λ")
#     for arg in e.argnames
#         print(io, arg, " ")
#     end
#     print(io, "-> ", e.body, ")")
# end
# Base.show(io::IO, e::Var) = print(io, e.name)
Base.show(io::IO, e::Prim) = print(io, e.name)
Base.show(io::IO, e::MetaVar) = printstyled(io, e.name; color=(e.idx%7)+1, bold=true)


function Base.show(io::IO, c::Corpus)
    for (i,(p,root)) in enumerate(zip(c.programs, c.roots))
        print(io, p.id, ": ", root)
        i < length(c.programs) && println(io)
    end
end

function Base.show(io::IO, n::CorpusNode)
    print(io, n.expr)
end