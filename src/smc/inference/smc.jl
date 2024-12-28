export smc


mutable struct Particle
    abs::Abstraction
    done::Bool
end
Base.copy(p::Particle) = Particle(p.abs, p.done)

mutable struct SMC
    particles::Vector{Particle}
    logweights::Vector{Float64}
    ancestors::Vector{Int}
end

Base.@kwdef struct Config
    num_particles::Int=3000
    seed::Union{Int,Nothing}=nothing
    verbose_best::Bool=false
    prefix::String="fn_"
    N::Int=1
    max_steps::Int=50
    temperature::Float64=.5
    utility_fn::Function=utility_by_rewrite
    logprob_mode::Bool=false
end

mutable struct SMCStats
    steps::Int
    proposals::Int
    expansions::Int
    time_smc::Float64
    time_rewrite::Float64
    matches_cache::HitRate
end
SMCStats() = SMCStats(0, 0, 0, 0., 0., HitRate())

mutable struct Shared
    matches_cache::Dict{PExpr, Abstraction}
    stats::SMCStats
end
Shared() = Shared(Dict{PExpr, Abstraction}(), SMCStats())

(Base.:+)(a::SMCStats, b::SMCStats) = SMCStats(a.steps + b.steps, a.proposals + b.proposals, a.expansions + b.expansions, a.time_smc + b.time_smc, a.time_rewrite + b.time_rewrite, a.matches_cache + b.matches_cache)

Base.show(io::IO, stats::SMCStats) = print(io, "SMCStats(steps=$(stats.steps), proposals=$(stats.proposals), expansions=$(stats.expansions), time_smc=$(round(stats.time_smc, sigdigits=2)), time_rewrite=$(round(stats.time_rewrite, sigdigits=2)), matches_cache=$(stats.matches_cache))")

struct SMCResult
    abstraction::Abstraction
    before::Corpus
    rewritten::Corpus
    stats::SMCStats
end

struct StitchResult
    original::Corpus
    steps::Vector{SMCResult}
    stats::SMCStats
end

function Base.show(io::IO, result::StitchResult)
    original_size = size(result.original)
    rewritten_size = size(result.steps[end].rewritten)
    ratio = original_size / rewritten_size
    println(io, "StitchResult(ratio=$(round(ratio, digits=2))x, original_size=$original_size, rewritten_size=$rewritten_size)")
    for (i, step) in enumerate(result.steps)
        println(io, "  ", step)
        # i < length(result.steps) && println(io)
    end
    print(io, "  ", result.stats)
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
    return StitchResult(original, results, sum(result.stats for result in results))
end

function smc(corpus::Corpus, config::Config, name::Symbol)

    tstart = time()
    @assert !has_prim(corpus, name) "Primitive $(name) already exists in corpus"

    init_abs = identity_abstraction(corpus, name)
    init_particle = Particle(init_abs, false)
    init_particles = Particle[copy(init_particle) for _ in 1:config.num_particles]
    init_logweights = fill(0., config.num_particles)
    init_ancestors = 1:config.num_particles
    smc = SMC(init_particles, init_logweights, init_ancestors)

    best_utility = 0.
    best_particle = init_particle
    shared = Shared()

    !isnothing(config.seed) && Random.seed!(config.seed)
    # println("seed: ", Random.seed!())

    while any(p -> !p.done, smc.particles)
        shared.stats.steps += 1

        shared.stats.steps > config.max_steps && break

        for (i, particle) in enumerate(smc.particles)
            particle.done && continue
            abs = sample_expansion(shared, particle.abs)
            
            particle.done = isnothing(abs)
            if !isnothing(abs)
                if isnan(abs.utility)
                    abs.utility = config.utility_fn(abs)
                    @assert !isnan(abs.utility)
                end
                particle.abs = abs
            end

            if particle.abs.utility > best_utility
                best_utility = particle.abs.utility
                best_particle = copy(particle)
                config.verbose_best && println("new best: ", particle.abs)
            end
        end

        all(p -> p.done, smc.particles) && break

        # resample
        for i in eachindex(smc.particles)
            particle = smc.particles[i]
            if config.logprob_mode
                smc.logweights[i] = particle.done ? -Inf : particle.abs.utility / config.temperature
            else
                smc.logweights[i] = particle.done ? -Inf : log(max(1., particle.abs.utility))
            end
        end

        smc.particles = resample_residual(smc.particles, smc.logweights)
        # particles = resample_multinomial(particles, weights)
    end

    shared.stats.time_smc = time() - tstart

    tstart = time()
    rewritten = rewrite(corpus, best_particle.abs)
    shared.stats.time_rewrite = time() - tstart

    return SMCResult(best_particle.abs, corpus, rewritten, shared.stats)
end


using Profile, PProf
export ptime, pprofile, pallocs
function ptime(f)
    Base.GC.gc()
    @time f()
end
function pprofile(f)
    ptime(f) # warmstart
    Base.GC.gc()
    Profile.clear()
    @profile f()
    pprof()
end
function pallocs(f; sample_rate=.001)
    ptime(f) # warmstart
    Base.GC.gc()
    Profile.Allocs.clear()
    Profile.Allocs.@profile sample_rate=.001 f()
    PProf.Allocs.pprof()
end


