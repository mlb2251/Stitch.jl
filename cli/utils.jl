function common_args(s)
    @add_arg_table s begin
        "--corpus"
        help = "Corpus of programs"
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
end
