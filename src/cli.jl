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

    @add_arg_table s begin
        "--application-utility-fixed"
        help = "Application utility fixed"
        default = Float32(-1.0)
        arg_type = Float32
    end

    @add_arg_table s begin
        "--application-utility-metavar"
        help = "Application utility metavar"
        default = Float32(0)
        arg_type = Float32
    end

    @add_arg_table s begin
        "--application-utility-symvar"
        help = "Application utility symvar"
        default = Float32(0)
        arg_type = Float32
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
        match_sequences=true,
        size_by_symbol=size_by_symbol,
        application_utility_fixed=args["application-utility-fixed"],
        application_utility_metavar=args["application-utility-metavar"],
        application_utility_symvar=args["application-utility-symvar"]
    )
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json([
        Dict([
            ("arity", abstraction.arity),
            ("sym_arity", abstraction.sym_arity),
            ("body", string(abstraction.body)),
            ("dfa_root", abstraction.dfa_root),
            ("dfa_metavars", abstraction.dfa_metavars),
            ("dfa_symvars", abstraction.dfa_symvars),
        ])
        for abstraction in abstractions
    ]))
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json([string(p) for p in corpus.programs]))
end

cli()