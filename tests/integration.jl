
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

function integrate(in_file, out_file)
    # read in the corpus
    corpus = load_corpus(in_file)
    abstractions, corpus = compress(corpus)
    abstractions = [abstraction_to_list(x) for x in abstractions]
    out = Dict(
        "abstractions" => abstractions,
        "programs" => [string(x) for x in corpus.programs],
    )
    if is_testing
        # canonicalize out by converting to JSON and back
        out = JSON.parse(JSON.json(out))
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
                if endswith(file, "-out.json")
                    continue
                end
                in_file = "data/$folder/$file"
                out_file = "data/$folder/$file-out.json"
                integrate(in_file, out_file)
            end
        end
    end
end