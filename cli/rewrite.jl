using Stitch
using ArgParse

import Stitch as S
import JSON

include("./utils.jl")

function rewrite()
    s = ArgParseSettings()
    @add_arg_table s begin
        "--abstractions"
        help = "Abstractions to use"
        arg_type = String
    end

    common_args(s)

    args = parse_args(s)

    corpus, kwargs = gather_common_arguments(args)
    abstractions = JSON.parse(args["abstractions"])
    abstractions = [parse(SExpr, p) for p in abstractions]

    rewritten, _ = rewrite_novel(
        corpus,
        abstractions;
        kwargs...
    )
    rewritten = rewritten.programs
    rewritten = [string(x) for x in rewritten]
    rewritten = JSON.json(rewritten)
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(rewritten)
end

rewrite()
