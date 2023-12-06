
using Stitch
using Test
import JSON

include("./framework.jl")

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