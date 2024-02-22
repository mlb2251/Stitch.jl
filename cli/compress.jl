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

    size_by_symbol_json = JSON.parse(args["size-by-symbol"])

    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(JSON.parse(args["corpus"]))])

    size_by_symbol = Dict(Symbol(k) => Float32(v) for (k, v) in size_by_symbol_json)
    dfa_valid_root_states = Set([Symbol(s) for s in JSON.parse(args["dfa-valid-root-states"])])
    kwargs = (;
        dfa=load_dfa(args["dfa"]),
        autoexpand_head=true,
        verbose_best=true,
        allow_single_task=false,
        max_arity=args["max-arity"],
        match_sequences=true,
        size_by_symbol=size_by_symbol,
        application_utility_fixed=args["application-utility-fixed"],
        application_utility_metavar=args["application-utility-metavar"],
        application_utility_symvar=args["application-utility-symvar"],
        dfa_valid_root_states=dfa_valid_root_states,
        dfa_metavariable_allow_S=!args["dfa-metavariable-disallow-S"],
        dfa_metavariable_allow_seqS=!args["dfa-metavariable-disallow-seqS"],
    )
    abstractions, corpus, _ = compress(
        corpus;
        iterations=args["iterations"],
        kwargs...
    )
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