using Stitch
using ArgParse

import Stitch as S
import JSON

include("./utils.jl")

function cli()
    # parse argument --iterations
    s = ArgParseSettings()
    @add_arg_table s begin
        "--iterations"
        help = "Number of iterations to run"
        default = 1
        arg_type = Int
    end

    common_args(s)

    args = parse_args(s)

    corpus, kwargs = gather_common_arguments(args)

    abstractions, corpus, _, corpus_sizes = compress(
        corpus;
        iterations=args["iterations"],
        kwargs...
    )
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(corpus_sizes)
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json([
        Dict([
            ("arity", abstraction.arity),
            ("sym_arity", abstraction.sym_arity),
            ("choice_arity", abstraction.choice_arity),
            ("body", string(abstraction.body)),
            ("dfa_root", abstraction.dfa_root),
            ("dfa_metavars", abstraction.dfa_metavars),
            ("dfa_symvars", abstraction.dfa_symvars),
            ("dfa_choicevars", abstraction.dfa_choicevars),
        ])
        for abstraction in abstractions
    ]))
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(JSON.json([string(p) for p in corpus.programs]))
end

cli()
