

is_testing = true

function abstraction_to_list(abstraction)
    return [
        abstraction.arity,
        abstraction.sym_arity,
        string(abstraction.body),
        abstraction.dfa_root,
        abstraction.dfa_metavars,
        abstraction.dfa_symvars,
    ]
end

function proc_args(args)
    args = Dict{Symbol,Any}(Symbol(k) => v for (k, v) in args)
    if :dfa in keys(args)
        args[:dfa] = load_dfa(args[:dfa])
    end
    if :size_by_symbol in keys(args)
        sbs = JSON.parsefile(args[:size_by_symbol])
        sbs = Dict{Symbol,Float32}(Symbol(k) => v for (k, v) in sbs)
        args[:size_by_symbol] = sbs
    end
    args
end

function integrate(in_file, out_file)
    # read in the corpus
    corpus = load_corpus(in_file)

    arguments = "$in_file-args.json"

    if isfile(arguments)
        argument_sets = JSON.parsefile(arguments)
    else
        argument_sets = [Dict()]
    end
    out = []
    for kwargs in argument_sets
        println(in_file, " ", kwargs)
        abstractions, compressed_corpus = compress(corpus; proc_args(kwargs)...)
        abstractions = [abstraction_to_list(x) for x in abstractions]
        out_per = Dict(
            "args" => kwargs,
            "abstractions" => abstractions,
            "programs" => [string(x) for x in compressed_corpus.programs],
        )
        push!(out, out_per)
    end
    if is_testing
        # canonicalize out by converting to JSON and back
        out = JSON.parse(JSON.json(out))
        println("Testing ", in_file)
        @test out == JSON.parsefile(out_file)
    else
        open(out_file, "w") do io
            JSON.print(io, out, 4)
        end
    end
end
