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

    @add_arg_table s begin
        "--dfa-valid-root-states"
        help = "Valid root states for the DFA"
        arg_type = String
        default = "[S, seqS, E]"
    end

    @add_arg_table s begin
        "--dfa-metavariable-disallow-S"
        help = "Disallow metavariables from being S"
        action = :store_true
    end

    @add_arg_table s begin
        "--dfa-metavariable-disallow-seqS"
        help = "Disallow metavariables from being seqS"
        action = :store_true
    end

    args = parse_args(s)

    size_by_symbol_json = JSON.parse(args["size-by-symbol"])

    line = readline()
    json = JSON.parse(line)
    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(json)])

    size_by_symbol = Dict(Symbol(k) => Float32(v) for (k, v) in size_by_symbol_json)
    dfa_valid_root_states = Set([Symbol(s) for s in JSON.parse(args["dfa-valid-root-states"])])
    abstractions, corpus, _ = compress_imperative(
        corpus,
        args["dfa"],
        iterations=args["iterations"],
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