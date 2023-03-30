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

    # @time stitch_search(corpus, S.utility_size_time_matches, S.upper_bound_sum_subtree_sizes, verbose=verbose, max_arity=2)
    @time stitch_search(corpus, S.upper_bound_sum_subtree_sizes, :fn_0; kwargs...)

end
