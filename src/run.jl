using Stitch
import Stitch as S
import JSON


function test(;kwargs...)

    # json = JSON.parsefile("data/cogsci/nuts-bolts.json")[1:5]
    json = JSON.parsefile("data/basic/simple1.json")

    corpus = Corpus([Program(parse(SExpr, p),i,i) for (i,p) in enumerate(json)])

    # @show corpus

    # @time stitch_search(corpus, S.utility_size_time_matches, S.upper_bound_sum_subtree_sizes, verbose=verbose, max_arity=2)
    @time stitch_search(corpus, S.upper_bound_sum_subtree_sizes, :fn_0; kwargs...)

end
