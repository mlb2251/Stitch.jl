

is_testing = true
strict = true

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
    shuf = nothing
    if :dfa in keys(args)
        args[:dfa] = load_dfa(args[:dfa])
    end
    if :size_by_symbol in keys(args)
        sbs = JSON.parsefile(args[:size_by_symbol])
        sbs = Dict{Symbol,Float32}(Symbol(k) => v for (k, v) in sbs)
        args[:size_by_symbol] = sbs
    end
    if :dfa_valid_root_states in keys(args)
        if args[:dfa_valid_root_states] == "any"
            args[:dfa_valid_root_states] = nothing
        else
            args[:dfa_valid_root_states] = Set([Symbol(x) for x in args[:dfa_valid_root_states]])
        end
    end
    if :dfa_start_state in keys(args)
        args[:dfa_start_state] = Symbol(args[:dfa_start_state])
    end
    if :shuf in keys(args)
        # delete the shuf key so it doesn't get passed to the search
        shuf = args[:shuf]
        delete!(args, :shuf)
    end
    args, shuf
end

function check_rewrite(corpus, compressed_corpus, abstractions; kwargs...)
    abstractions = [abs.body for abs in abstractions]
    for i in 1:length(corpus.programs)
        limited_corpus = Corpus([corpus.programs[i]])
        result = rewrite_novel(limited_corpus, abstractions; kwargs...)[1].programs[1]
        expected = compressed_corpus.programs[i]
        @test string(result) == string(expected)
    end
end

function compute(corpus, kwargs, kwargs_specific; seed=nothing)
    # to_follow = parse(SExpr, "(/subseq a b c d e f a b c d e f)")
    # abstractions, compressed_corpus, _ = compress(corpus; strict=strict, shuffle_expansions_seed=seed, kwargs_specific..., follow=true, track=to_follow)
    abstractions, compressed_corpus, _ = compress(corpus; strict=strict, shuffle_expansions_seed=seed, kwargs_specific...)
    check_rewrite(corpus, compressed_corpus, abstractions; kwargs_specific...)
    abstractions = [abstraction_to_list(x) for x in abstractions]
    return Dict(
        "args" => kwargs,
        "abstractions" => abstractions,
        "programs" => [string(x) for x in compressed_corpus.programs],
    )
end

function integrate(in_file, out_file)

    printstyled("Testing $in_file\n", color=:blue, bold=true)
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
        kwargs_specific, shuf = proc_args(kwargs)
        out_per = compute(corpus, kwargs, kwargs_specific)
        if !isnothing(shuf)
            for seed in 1:shuf
                println("seed ", seed)
                out_shufd = compute(corpus, kwargs, kwargs_specific; seed=seed)
                @test out_shufd == out_per
            end
        end
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

function folder_tests(folder)
    @testset "$folder" begin
        for file in readdir("data/$folder")
            if !endswith(file, ".json")
                continue
            end
            if endswith(file, "-out.json") || endswith(file, "-args.json")
                continue
            end
            in_file = "data/$folder/$file"
            out_file = "data/$folder/$file-out.json"
            integrate(in_file, out_file)
        end
    end
end

function full_tests()
    @testset "integration" begin
        # one test set for each folder in data/
        for folder in readdir("data")
            if !isdir("data/$folder")
                continue
            end
            folder_tests(folder)
        end
    end
end