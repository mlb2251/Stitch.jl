module Stitch

export SExpr, stitch_search, Corpus, Program, compress, load_corpus, load_dfa, rewrite_novel, intermediate_search_results, SearchConfig
export bottom_up_utility, SearchState, is_leaf, is_hole, find_holes, compute_best_score_and_lower_bound, bounds_analysis

using AutoHashEquals

function pretty_show(io::IO, obj; indent=false)
    delim = if indent
        "\n    "
    else
        ", "
    end
    print(io, typeof(obj), "(")
    indent && print(io, delim)
    for k in fieldnames(typeof(obj))
        print(io, k, "=", getfield(obj, k), delim)
    end
    print(io, ")")
end


include("expr.jl")
include("search.jl")
include("expand.jl")
include("utility.jl")
include("rewrite.jl")
include("analysis.jl")

end # module Stitch


