mutable struct JSONLogger
    state::SMC
    config::Config
    history::Vector{Any}
    log_proposal_ratios::Vector{Float64}
    loglikelihood_ratios::Vector{Float64}
    logprior_ratios::Vector{Float64}
    weight_incrs::Vector{Float64}
    highlight_paths::Vector{Path}
    expr_ids::IdSet{String}
end

function JSONLogger(smc::SMC, config::SMCConfig)
    JSONLogger(smc, config, [], fill(0.0, config.num_particles), fill(0.0, config.num_particles), fill(0.0, config.num_particles), fill(0.0, config.num_particles), fill(Path[], config.num_particles), IdSet{String}())
end

function log_init!(logger)
    push!(logger.history, json_state(logger, 0, :init))
end

function log_smc_step!(logger, step)
    push!(logger.history, json_state(logger, step, :smc_step))
end

function log_resample!(logger, step, ancestors)
    push!(logger.history, json_state(logger, step, :resample, ancestors=ancestors))
end

function json_state(logger, step, mode; ancestors=nothing)
    state = logger.state
    Dict(
        :step => step,
        :mode => mode,
        :ancestors => ancestors,
        :fieldnames => ["expr", "likelihood", "prior", "posterior", "logweight", "weight_incr", "log_proposal_ratio", "loglikelihood_ratio", "logprior_ratio"],
        :particles => [
            [
                logger.expr_ids[mode == :init ? "<<<>>>$(string(state.particles[i].expr.expr.child))" : highlight_subexpression(state.particles[i].expr.expr.child, logger.highlight_paths[i], "<<<", ">>>")],
                round3(exp(state.particles[i].loglikelihood)),
                round3(exp(state.particles[i].logprior)),
                round3(exp(state.particles[i].logposterior)),
                round3(state.logweights[i]),
                mode == :resample ? nothing : round3(logger.weight_incrs[i]),
                mode == :resample ? nothing : round3(logger.log_proposal_ratios[i]),
                mode == :resample ? nothing : round3(logger.loglikelihood_ratios[i]),
                mode == :resample ? nothing : round3(logger.logprior_ratios[i]),
            ]
            for i in eachindex(state.particles)
        ]
    )
end