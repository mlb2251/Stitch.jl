using Stitch
using ArgParse

import Stitch as S
using JSON: JSON

repeats = 10
iterations = 1

function benchmark(path::String, size_by_symbol, config_args)
	println(path)
	corpus = load_corpus(path)
	result = () -> compress(
		corpus;
		iterations = iterations,
		size_by_symbol=size_by_symbol,
		config_args...,
	)
	result()
	repeated = []
	for _ in 1:repeats
		start_t = time_ns()
		abstractions, _, dfa, corpus_sizes = result()
		end_t = time_ns()
		time_taken = (end_t - start_t) / 1e9
		push!(
			repeated,
			Dict(
				"time_taken" => time_taken,
				"corpus_sizes" => corpus_sizes,
				"abstraction_sizes" => [
					S.size(abstraction.body, size_by_symbol)
					for abstraction in abstractions
				],
				"abstractions" => [
					string(abstraction.body)
					for abstraction in abstractions
				]
			),
		)
	end
    return repeated
end

function main(folder, shortname, size_by_symbol, config_args; predicate=x -> true)
    results_all = []
    for path in readdir(folder)
        if endswith(path, ".json") && !contains(path, "-out") && predicate(path)
            result = benchmark(joinpath(folder, path), size_by_symbol, config_args)
            push!(results_all,
                Dict(
                    "path" => path,
                    "result" => result,
                )
            )
        end
    end
    # write to file
    open("analysis_out/$(shortname)-benchmark-results.json", "w") do io
        JSON.print(io, results_all, 2)
    end
end

# main("../compression_benchmark/processed/without-apps-no-lam", "without-apps", nothing, ())
# main("../compression_benchmark/processed/with-apps-no-lam", "with-apps", Dict(:app => Float32(0.01)), ())
for max_arity in 3:5
	main("../compression_benchmark/processed/without-apps-no-lam", "without-apps-arity=$(max_arity)", Dict(:app => Float32(0.01)), (;max_arity=max_arity); predicate=(path -> path == "wheels.json"))
end