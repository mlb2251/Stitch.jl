module Stitch

export SExpr, stitch_search, Corpus, Program, compress, load_corpus, load_dfa, rewrite_novel, bounds_analysis

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


