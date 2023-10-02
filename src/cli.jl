using Stitch
using ArgParse

import Stitch as S
import JSON


function cli()
    # parse argument --iterations
    s = ArgParseSettings()
    @add_arg_table s begin
        "--iterations"
            help="Number of iterations to run"
            default=1
            arg_type=Int
    end

    args = parse_args(s)
    line = readline()
    json = JSON.parse(line)
    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(json)])
    result = compress(corpus, iterations=args["iterations"])
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json(result))
end

cli()