export smc


mutable struct Particle
    abs::Abstraction
    weight::Float64
    done::Bool
end
Base.copy(p::Particle) = Particle(copy(p.abs), p.weight, p.done)


function smc(; path="data/cogsci/nuts-bolts.json", seed=nothing, num_particles=3000, name::Symbol=:fn)
    programs = String.(JSON.parsefile(path))
    corpus = Corpus(programs)

    @assert !has_prim(corpus, name) "Primitive $name already exists in corpus"

    init_abs = identity_abstraction(corpus, name)
    init_particle = Particle(init_abs, 0., false)
    particles = Particle[copy(init_particle) for _ in 1:num_particles]

    # tmp_particles = Particle[copy(init_particle) for _ in 1:num_particles]


    best_utility = 0.
    best_particle = init_particle
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
                best_particle = copy(particle)
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

    rewritten = rewrite(corpus, best_particle.abs)
    return rewritten
end


