using Stitch
import Stitch as S
import JSON


function test()

    json = JSON.parsefile("data/cogsci/nuts-bolts.json")
    # json = JSON.parsefile("data/basic/simple1.json")

    corpus = Corpus([Program(parse(SExpr, p),i,i) for (i,p) in enumerate(json)])

    # @show corpus

    stitch_search(corpus, S.utility_size_time_matches, S.upper_bound_inf, verbose=false)

end
