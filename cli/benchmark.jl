using Stitch
using ArgParse

import Stitch as S
using JSON: JSON

repeats = 10
iterations = 1

function benchmark(path::String)
	println(path)
	corpus = load_corpus(path)
	result = () -> compress(
		corpus;
		iterations = iterations,
	)
	result()
	repeated = []
	for _ in 1:repeats
		start_t = time_ns()
		abstractions, corpus, dfa, corpus_sizes = result()
		end_t = time_ns()
		time_taken = (end_t - start_t) / 1e9
		push!(
			repeated,
			Dict(
				"time_taken" => time_taken,
				"corpus_sizes" => corpus_sizes,
				"abstraction_sizes" => [
					S.size(abstraction.body, nothing)
					for abstraction in abstractions
				],
			),
		)
	end
    return repeated
end

function main()
    results_all = []
    for path in readdir("data/cogsci")
        if endswith(path, ".json") && !contains(path, "-out")
            result = benchmark(joinpath("data/cogsci", path))
            push!(results_all, [
                Dict(
                    "path" => path,
                    "result" => result,
                )
            ])
        end
    end
    # write to file
    open("analysis_out/cogsci-benchmark-results.json", "w") do io
        JSON.print(io, results_all)
    end
end

main()