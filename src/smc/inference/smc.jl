export smc


mutable struct Particle
    abs::Abstraction
    weight::Float64
    done::Bool
end
Base.copy(p::Particle) = Particle(copy(p.abs), p.weight, p.done)


Base.@kwdef struct Config
    num_particles::Int=3000
    seed::Union{Int,Nothing}=nothing
    verbose_best::Bool=false
    prefix::String="fn_"
    N::Int=1
end

struct SMCResult
    abstraction::Abstraction
    before::Corpus
    rewritten::Corpus
end

struct StitchResult
    original::Corpus
    steps::Vector{SMCResult}
end

function Base.show(io::IO, result::StitchResult)
    original_size = size(result.original)
    rewritten_size = size(result.steps[end].rewritten)
    ratio = original_size / rewritten_size
    println(io, "StitchResult(ratio=$(round(ratio, digits=2))x, original_size=$original_size, rewritten_size=$rewritten_size)")
    for (i, step) in enumerate(result.steps)
        print(io, "  ", step)
        i < length(result.steps) && println(io)
    end
end

function Base.show(io::IO, result::SMCResult)
    ratio = size(result.before) / size(result.rewritten)
    print(io, round(ratio, digits=2), "x ", result.abstraction)
end

function compress(path::String; kwargs...)
    compress(load_corpus(path); kwargs...)
end

function cogsci(; kwargs...)
    paths = [
        "data/cogsci/nuts-bolts.json",
        # "data/cogsci/bridge.json",
        "data/cogsci/dials.json",
        "data/cogsci/furniture.json",
        # "data/cogsci/house.json",
        "data/cogsci/wheels.json",
        # "data/cogsci/city.json",
        # "data/cogsci/castle.json",
    ]
    for path in paths
        println(path)
        @time result = compress(path; kwargs...)
        println(result)
    end
end




function compress(corpus::Corpus; kwargs...)
    config = Config(;kwargs...)
    original = corpus
    results = SMCResult[]
    for i in 1:config.N
        name = Symbol(config.prefix, i)
        result = smc(corpus, config, name)
        push!(results, result)
        corpus = result.rewritten
    end
    return StitchResult(original, results)
end

function smc(corpus::Corpus, config::Config, name::Symbol)

    @assert !has_prim(corpus, name) "Primitive $(name) already exists in corpus"

    init_abs = identity_abstraction(corpus, name)
    init_particle = Particle(init_abs, 0., false)
    particles = Particle[copy(init_particle) for _ in 1:config.num_particles]

    # tmp_particles = Particle[copy(init_particle) for _ in 1:num_particles]


    best_utility = 0.
    best_particle = init_particle
    temperature = .5

    !isnothing(config.seed) && Random.seed!(config.seed)
    # @show Random.seed!()

    while any(p -> !p.done, particles)
        for particle in particles
            # println("expand")
            res = sample_expansion!(particle.abs)
            particle.done = !res
            if particle.abs.utility > best_utility
                best_utility = particle.abs.utility
                best_particle = copy(particle)
                config.verbose_best && println("new best: ", particle.abs)
            end
        end


        # resample
        for particle in particles
            particle.weight = max(1., particle.abs.utility)
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

        @inbounds particles = [copy(particles[sample_normalized(weights)]) for _ in 1:config.num_particles]
    end

    rewritten = rewrite(corpus, best_particle.abs)

    return SMCResult(best_particle.abs, corpus, rewritten)
end

