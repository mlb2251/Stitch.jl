using Stitch
using ArgParse

import Stitch as S
using JSON: JSON

repeats = 10
iterations = 1

function benchmark(path::String, size_by_symbol)
	println(path)
	corpus = load_corpus(path)
	result = () -> compress(
		corpus;
		iterations = iterations,
		size_by_symbol=size_by_symbol
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

function main(folder, shortname, size_by_symbol)
    results_all = []
    for path in readdir(folder)
        if endswith(path, ".json") && !contains(path, "-out")
            result = benchmark(joinpath(folder, path), size_by_symbol)
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

main("../compression_benchmark/processed/without-apps", "without-apps", nothing)
main("../compression_benchmark/processed/with-apps", "with-apps", Dict(:app => Float32(0.01)))
