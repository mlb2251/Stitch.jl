using Stitch
using JSON


"""
Every operator in the tower dsl takes as a final argument a continuation for what code to run after 

input example:  (lambda (tower_loopM 6 (lambda (lambda (reverseHand (tower_embed (lambda (moveHand 2 (1x3 \$0))) \$0)))) (3x1 \$0)))

instead of everything being (state -> state) -> (state -> state) everything is just (state -> state)
(lambda (moveHand 2 (1x3 \$0))) -> (/seq (moveHand 2) (1x3))


(lambda (tower_embed (lambda (moveHand 2 (1x3 \$0))))) -> (lambda (tower_embed (lambda (/seq (moveHand 2) 1x3)))


(lambda e) -> (translateSeq (downshift e))
translateSeq e = (/seq (translate e)...) # translateSeq produces a seq of a list of expressions (splatted out list) where each expression has type (state -> state)
translate (1x3 k) -> 1x3 :: (translate k) # `::` is cons. continuation k becomes sequencing. Instead of 1x3 :: (state -> state) -> (state -> state), it's now (1x3 :: state -> state)
translate (3x1 k) -> 3x1 :: (translate k)
translate (reverseHand k) -> reverseHand :: (translate k)
translate (moveHand i k) -> (moveHand i) :: (translate k)

# (tower_embed body) duplicates its pen state, runs the body seq on it, and then restores the pen state
# the old lambda used to just get fed the identity continuation I think
translate (tower_embed (lambda body) k) -> (tower_embed (translateSeq (downshift body))) :: (translate k)

# (tower_loopM i (lambda body)) runs the body (which takes a loop index and produces a seq of seqs) i times. First it runs the body, then it runs the result of the body, etc.
# and the inner lambda is the one with the continuation and outer one is the index. So just downshfit once.
translate (tower_loopM i (lambda (lambda body)) k) -> (tower_loopM i (lambda (translateSeq (downshift body)))) :: (translate k)

(translate \$i) -> () # this just helps eliminate all those empty continuations like how (1x3 \$0) should become 1x3. You can assert that these got downshifted to <0 I think thats true since we eliminated all their lambda.

"""

function translate_program(sexpr::SExpr)::SExpr
    # println("translate_program ", sexpr)
    # (lambda e) -> (translateSeq (downshift e))
    @assert sexpr.children[1].leaf == :lambda
    body = sexpr.children[2]
    return parse(SExpr, translateSeq(downshift(body, 0)))
end

function translateSeq(sexpr::SExpr)::String
    return "(/seq " * translate(sexpr) * ")"
end

function translate(sexpr::SExpr)::String
    if is_leaf(sexpr)
        if startswith(string(sexpr.leaf), "\$")
            # translate $i -> () if i < 0, else "$i"
            idx = parse(Int, string(sexpr.leaf)[2:end])
            return idx < 0 ? "" : string(sexpr.leaf)
        end
        return string(sexpr.leaf)
    end
    
    head = sexpr.children[1].leaf
    args = sexpr.children[2:end]

    if head === Symbol("1x3")
        # translate (1x3 k) -> 1x3 :: (translate k)
        # `::` is cons, continuation k becomes sequencing
        # Instead of 1x3 :: (state -> state) -> (state -> state), it's now (1x3 :: state -> state)    
        return "1x3 " * translate(args[1])
    elseif head === Symbol("3x1")
        # translate (3x1 k) -> 3x1 :: (translate k)
        return "3x1 " * translate(args[1])
    elseif head === :reverseHand
        # translate (reverseHand k) -> reverseHand :: (translate k)
        return "reverseHand " * translate(args[1])
    elseif head === :moveHand
        # translate (moveHand i k) -> (moveHand i) :: (translate k)
        return "(moveHand " * string(args[1].leaf) * ") " * translate(args[2])
    elseif head === :tower_embed
        # translate (tower_embed (lambda body) k) -> (tower_embed (translateSeq (downshift body))) :: (translate k)
        @assert args[1].children[1].leaf == :lambda
        body = args[1].children[2]
        return "(tower_embed " * translateSeq(downshift(body, 0)) * ") " * translate(args[2])
    elseif head === :tower_loopM
        # translate (tower_loopM i (lambda (lambda body)) k) -> (tower_loopM i (lambda (translateSeq (downshift body)))) :: (translate k)
        @assert args[2].children[1].leaf == :lambda
        @assert args[2].children[2].children[1].leaf == :lambda
        i = args[1].leaf
        body = args[2].children[2].children[2]
        return "(tower_loopM " * string(i) * " (lambda " * translateSeq(downshift(body, 0)) * ")) " * translate(args[3])
    else
        # error
        error("Unknown head: $head")
    end
end

function downshift(sexpr::SExpr, cutoff::Int)::SExpr
    # downshift $4->$3
    if is_leaf(sexpr)
        if !startswith(string(sexpr.leaf), "\$")
            return copy(sexpr) # non-var leaf
        end
        # var leaf
        idx = parse(Int, string(string(sexpr.leaf)[2:end]))
        if idx > cutoff
            # no downshift if this var corresponds to a lambda thats way above us
            return copy(sexpr) # for example if cutoff is 0, we do shift $0 but we dont shift $1
        end
        # downshift
        new_leaf = copy(sexpr)
        new_leaf.leaf = Symbol("\$" * string(idx - 1))
        return new_leaf
    end

    # if we cross a lambda, increment cutoff
    if sexpr.children[1].leaf == :lambda
        cutoff += 1
    end

    # recurse
    return sexpr_node([copy(sexpr.children[1]), [downshift(child, cutoff) for child in sexpr.children[2:end]]...])
end


function towers_to_seq(;file="compression_benchmark/benches/towers_tower_batch_50_3600_ellisk_2019-03-26T10.51.16/bench000_it0.json")
    dc_json = JSON.parsefile(file)
    frontiers = [frontier["programs"] for frontier in dc_json["frontiers"]]
    programs = []
    for (task_id, frontier) in enumerate(frontiers)
        for program in frontier
            sexpr = parse(SExpr, program["program"])
            translated = translate_program(sexpr)
            push!(programs, Program(translated, length(programs) + 1, task_id))
        end
    end

    corpus = Corpus(programs)
    abstractions, corpus, dfa, _ = compress(corpus; iterations=10, max_arity=2, match_sequences=true)
    return abstractions
end



function threading()
    programs = [
        "(foo foo foo (lam (bar bar bar (inc \$0))))",
        "(foo foo foo (lam (bar bar bar (+ \$0 \$0))))",
        ]
    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(programs)])
    abstractions, corpus, dfa = compress(corpus; iterations=1, max_arity=2, match_sequences=true)
    println(abstractions[1])
    println(corpus)
end


function maddy(; programs= "data/cogsci/bridge.json")
    programs = JSON.parsefile(programs)[1:40]

    programs = ["(/seq " * p[2:end] for p in programs]

    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(programs)])

    abstractions, corpus, dfa, _ = compress(corpus; iterations=10, max_arity=2, match_sequences=true)
    # abstractions, corpus, dfa = compress(corpus; iterations=1, max_arity=2, match_sequences=true, follow=true, track=parse(SExpr, "(/subseq t (r 10) t (l 9) h (r 2) h (r 6) h (l 2) h (l 6) h (r 4) h (r 4) h)"))

    abstractions
end


function maddy2(; programs= "data/cogsci/nuts-bolts.json")
    programs = JSON.parsefile(programs)

    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(programs)])

    abstractions, corpus, dfa = compress(corpus; iterations=1, max_arity=3, match_sequences=false)
    # abstractions, corpus, dfa = compress(corpus; iterations=1, max_arity=2, match_sequences=true, follow=true, track=parse(SExpr, "(/subseq t (r 10) t (l 9) h (r 2) h (r 6) h (l 2) h (l 6) h (r 4) h (r 4) h)"))

    abstractions
end


using Profile
using PProf
export @prof
using JSON

ADDR::String = "http://localhost"
PORT::Int = 8001



macro prof(ex)
    out = "profile.pb.gz"
    out_allocs = "alloc-profile.pb.gz"
    return quote
        printstyled("Warmstart\n"; color=:gray, bold=true)
        $(esc(ex))
        printstyled("Time\n"; color=:gray, bold=true)
        @time $(esc(ex))
        # printstyled("Benchmark\n"; color=:gray, bold=true)
        # QuoteNode(BenchmarkTools.@btime $ex)
        
        printstyled("Profiling Time\n"; color=:yellow, bold=true)
        Profile.clear()
        @time Profile.@profile $(esc(ex))
        PProf.pprof(; web=false, out=$(out))
        show_prof(; file=$(out))

        printstyled("Profiling Allocs\n"; color=:blue, bold=true)
        Profile.Allocs.clear()
        @time Profile.Allocs.@profile $(esc(:sample_rate)) = 0.003 $(esc(ex))
        PProf.Allocs.pprof(; web=false, out=$(out_allocs))
        show_prof(; file=$(out_allocs))
    end
end


const procs = Vector{Base.Process}()


function kill_servers()
    println("killing servers")
    for proc in procs
        Base.kill(proc)
    end
end

function show_prof(;
    file="profile.pb.gz",
    webhost="localhost",
    port="", # random open port
    ui_relative_percentages::Bool=true
)
    # The first time, register an atexit hook to kill the web servers
    isempty(procs) && atexit(kill_servers)

    relative_percentages_flag = ui_relative_percentages ? "-relative_percentages" : ""

    push!(procs, PProf.pprof_jll.pprof() do pprof_path
        open(pipeline(`$pprof_path -http=$webhost:$port $relative_percentages_flag $file`))
    end)
    # sleep(0.1) # convenience – give time for the output to show in the terminal
    nothing
end

