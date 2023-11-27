
using Stitch
using Test
import JSON

is_testing = true

function abstraction_to_list(abstraction)
    return [
        abstraction.arity,
        abstraction.sym_arity,
        string(abstraction.body),
        abstraction.dfa_root,
        abstraction.dfa_metavars,
        abstraction.dfa_symvars,
    ]
end

function proc_args(args)
    args = Dict(Symbol(k) => v for (k, v) in args)
    args
end

function integrate(in_file, out_file)
    # read in the corpus
    corpus = load_corpus(in_file)

    arguments = "$in_file-args.json"

    if isfile(arguments)
        argument_sets = JSON.parsefile(arguments)
        argument_sets = [proc_args(args) for args in argument_sets]
    else
        argument_sets = [Dict()]
    end
    out = []
    for kwargs in argument_sets
        abstractions, compressed_corpus = compress(corpus; kwargs...)
        abstractions = [abstraction_to_list(x) for x in abstractions]
        out_per = Dict(
            "args" => kwargs,
            "abstractions" => abstractions,
            "programs" => [string(x) for x in compressed_corpus.programs],
        )
        push!(out, out_per)
    end
    if is_testing
        # canonicalize out by converting to JSON and back
        out = JSON.parse(JSON.json(out))
        println("Testing ", in_file)
        @test out == JSON.parsefile(out_file)
    else
        open(out_file, "w") do io
            JSON.print(io, out, 4)
        end
    end
end

@testset "integration" begin
    # one test set for each folder in data/
    for folder in readdir("data")
        if !isdir("data/$folder")
            continue
        end
        @testset "$folder" begin
            for file in readdir("data/$folder")
                if !endswith(file, ".json")
                    continue
                end
                if endswith(file, "-out.json") || endswith(file, "-args.json")
                    continue
                end
                in_file = "data/$folder/$file"
                out_file = "data/$folder/$file-out.json"
                integrate(in_file, out_file)
            end
        end
    end
end