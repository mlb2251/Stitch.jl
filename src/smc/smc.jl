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

const production_ids = Dict{Production, Int}()
const expr_ids = Dict{PExpr, Int}()


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

function make_corpus_nodes(expr::PExpr, program::ProgramInfo)
    # if expr isa Abs
    #     # we skip over lambdas because you cant match at them
    #     return CorpusNode(expr.body, program)
    # end

    expr_id = get!(expr_ids, expr, length(expr_ids)+1)
    prod = Production(expr)
    prod_id = get!(production_ids, prod, length(production_ids)+1)

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


const Path = Vector{Int}


# struct Match
#     node::CorpusNode
#     metavar_args::Vector{CorpusNode}
# end

# Base.copy(m::Match) = Match(m.node, copy(m.metavar_args)) # can keep shallow


mutable struct MetaVarPath
    path::Path
    idx::Int
    frozen::Bool
end
Base.copy(m::MetaVarPath) = MetaVarPath(copy(m.path), m.idx, m.frozen)

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
end

# Base.copy(a::Abstraction) = Abstraction(Match[copy(m) for m in a.matches], copy.(a.metavar_paths), copy(a.expr), a.fresh_metavar, a.size, a.utility)
Base.copy(a::Abstraction) = Abstraction(copy(a.matches), MetaVarPath[copy(p) for p in a.metavar_paths], copy(a.expr), a.fresh_metavar, a.size, a.multiuses, a.utility, a.arity)


getchild(node::CorpusNode, path::MetaVarPath)::CorpusNode = getchild(node, path.path)
function getchild(node::CorpusNode, path::Path)::CorpusNode
    for i in path
        node = node.children[i]
    end
    return node
end

getchild(node::PExpr, path::MetaVarPath)::PExpr = getchild(node, path.path)
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

setchild!(node::PExpr, path::MetaVarPath, child::PExpr) = setchild!(node, path.path, child)

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
    all(p -> p.frozen, abs.metavar_paths) && return false

    # pick a random match location to use as the basis for expansion
    match = abs.matches[rand(1:end)]

    # pick a random path to consider expanding
    probs = fill(1., length(abs.metavar_paths))
    for (i, path) in enumerate(abs.metavar_paths)
        if path.frozen
            probs[i] = 0.
        end
    end
    probs ./= sum(probs)

    i = sample_normalized(probs)
    path = abs.metavar_paths[i]
    child_i = getchild(match, path)

    # should we do multiuse or syntactic expansion? Lets pick something that makes sense here. So lets check for multiuse
    # loop over all pairs of metavar_paths with this one


    if rand() < 0.5
        # consider multiuse expansion
        multiuse_candidates = filter(eachindex(abs.metavar_paths)) do j
            j != i && child_i.expr_id == getchild(match, abs.metavar_paths[j]).expr_id
        end

        if length(multiuse_candidates) > 0
            # pick a random one
            j = multiuse_candidates[rand(1:end)]
            multiuse_expansion!(abs, match, i, path, child_i, j)
            return true
        end
    end

    syntactic_expansion!(abs, match, i, path, child_i)

    return true
end

function syntactic_expansion!(abs::Abstraction, match::CorpusNode, i::Int, path_i::MetaVarPath, child_i::CorpusNode)

    popat!(abs.metavar_paths, i);

    # subset to the matches
    filter!(abs.matches) do node
        child_i.production_id == getchild(node, path_i).production_id
    end

    # grow the abstraction
    prod = child_i.production
    if prod.type === :app
        new_expr = App(prod.head, PExpr[])
        for j in 1:prod.argc
            push!(new_expr.args, MetaVar(abs.fresh_metavar))
            new_path = MetaVarPath(copy(path_i.path), abs.fresh_metavar, false)
            push!(new_path.path, j)
            push!(abs.metavar_paths, new_path)
            abs.fresh_metavar += 1
            abs.arity += 1
        end
    else
        new_expr = prod.head
    end
    abs.expr = setchild!(abs.expr, path_i, new_expr)
    abs.size += 1
    abs.arity -= 1
    abs.utility = utility(abs)
end

utility(abs::Abstraction) = length(abs.matches)*(abs.size + abs.multiuses*.9)


function multiuse_expansion!(abs::Abstraction, match::CorpusNode, i::Int, path_i::MetaVarPath, child_i::CorpusNode, j::Int)
    path_j = abs.metavar_paths[j]

    # set i (non-frozen) to j (may or may not be frozen) and freeze both
    abs.metavar_paths[i].frozen = true
    abs.metavar_paths[j].frozen = true
    abs.metavar_paths[i].idx = path_j.idx

    # subset to the matches
    filter!(abs.matches) do node
        getchild(node, path_i).expr_id == getchild(node, path_j).expr_id
    end

    # set the two vars to be the same. `j` is the one that will be kept since it might already be frozen
    abs.expr = setchild!(abs.expr, path_i, MetaVar(path_j.idx))
    abs.multiuses += 1
    abs.arity -= 1
    abs.utility = utility(abs)
end




function identity_abstraction(corpus)
    nodes = descendants(corpus)
    # return Abstraction([Match(node, CorpusNode[node]) for node in nodes], Path[Int[]], MetaVar(1, metavar_names[1]), 2, 0, 0.)
    metavar_idx = 1
    return Abstraction(nodes, MetaVarPath[MetaVarPath(Path(), metavar_idx, false)], MetaVar(metavar_idx), metavar_idx+1, 0, 0, 0., 1,)
end

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


mutable struct Particle
    abs::Abstraction
    weight::Float64
    done::Bool
end
Base.copy(p::Particle) = Particle(copy(p.abs), p.weight, p.done)

function smc(; path="data/cogsci/nuts-bolts.json", seed=nothing, num_particles=3000)
    programs = String.(JSON.parsefile(path))
    corpus = Corpus(programs)

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
            particle.weight = particle.abs.utility
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
Base.show(io::IO, e::MetaVar) = printstyled(io, metavar_names[e.idx]; color=(e.idx%6)+1, bold=true)


function Base.show(io::IO, c::Corpus)
    for (i,(p,root)) in enumerate(zip(c.programs, c.roots))
        print(io, p.id, ": ", root)
        i < length(c.programs) && println(io)
    end
end

function Base.show(io::IO, n::CorpusNode)
    print(io, n.expr)
end