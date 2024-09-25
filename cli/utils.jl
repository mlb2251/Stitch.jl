function common_args(s)
    @add_arg_table s begin
        "--corpus"
        help = "Corpus of programs"
        arg_type = String
    end

    @add_arg_table s begin
        "--corpus-file"
        help = "Corpus of programs stored in this file"
        arg_type = String
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

    @add_arg_table s begin
        "--dfa-metavariable-allow-anything"
        help = "Allow metavariables to be anything"
        action = :store_true
    end
end

function gather_common_arguments(args)
    size_by_symbol_json = JSON.parse(args["size-by-symbol"])

    if args["corpus"] !== nothing
        corpus = args["corpus"]
    else
        @assert args["corpus-file"] !== nothing
        corpus = read(args["corpus-file"], String)
    end

    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(JSON.parse(corpus))])

    size_by_symbol = Dict(Symbol(k) => Float32(v) for (k, v) in size_by_symbol_json)
    if args["dfa-valid-root-states"] === "any"
        dfa_valid_root_states = nothing
    else
        dfa_valid_root_states = Set([Symbol(s) for s in JSON.parse(args["dfa-valid-root-states"])])
    end
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
        dfa_metavariable_allow_anything=args["dfa-metavariable-allow-anything"],
    )
    return corpus, kwargs
end
