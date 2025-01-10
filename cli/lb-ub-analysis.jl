
using Stitch
using Test
import JSON

function main(input_file, output_file, count)
    corpus = load_corpus(input_file)
    res = [[string(prog), util_ub, util_real, util_lb] for (prog, util_ub, util_real, util_lb) in bounds_analysis(corpus, count)]
    text = JSON.json(res)
    open(output_file, "w") do io
        write(io, text)
    end
end

main("data/cogsci/dials.json", "analysis_out/dials_sample.json", 200)
main("data/cogsci/wheels.json", "analysis_out/wheels_sample.json", 200)
main("data_slow/A.json", "analysis_out/A_sample.json", 200)
