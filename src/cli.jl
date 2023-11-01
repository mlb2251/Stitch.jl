using Stitch
using ArgParse

import Stitch as S
import JSON


function cli()
    # parse argument --iterations
    s = ArgParseSettings()
    @add_arg_table s begin
        "--iterations"
        help = "Number of iterations to run"
        default = 1
        arg_type = Int
    end

    @add_arg_table s begin
        "--max-arity"
        help = "Maximum arity of abstractions"
        default = 2
        arg_type = Int
    end

    @add_arg_table s begin
        "--dfa"
        help = "DFA to use for parsing"
        arg_type = String
    end

    @add_arg_table s begin
        "--size-by-symbol"
        help = "Size of each symbol"
        arg_type = String
    end


    args = parse_args(s)

    size_by_symbol_json = JSON.parse(args["size-by-symbol"])

    line = readline()
    json = JSON.parse(line)
    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(json)])

    size_by_symbol = Dict(Symbol(k) => Float32(v) for (k, v) in size_by_symbol_json)
    abstractions, corpus = compress_imperative(
        corpus,
        args["dfa"],
        iterations=args["iterations"],
        max_arity=args["max-arity"],
        size_by_symbol=size_by_symbol
    )
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json([
        Dict([("arity", abstraction.arity), ("sym_arity", abstraction.sym_arity), ("body", string(abstraction.body))])
        for abstraction in abstractions
    ]))
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json([string(p) for p in corpus.programs]))
end

cli()