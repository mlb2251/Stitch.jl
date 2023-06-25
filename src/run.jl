using Stitch
import Stitch as S
import JSON


function test(;file="data/cogsci/nuts-bolts.json", truncate=nothing, kwargs...)

    # json = JSON.parsefile("data/cogsi/city.json")

    json = JSON.parsefile(file)#[1:100]
    if !isnothing(truncate)
        json = json[1:truncate]
    end
    # json = JSON.parsefile("data/basic/simple1.json")

    corpus = Corpus([Program(parse(SExpr, p),i,i) for (i,p) in enumerate(json)])

    # @show corpus

    @time compress(corpus; kwargs...)

end

