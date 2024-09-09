
using Stitch
using JSON

function maddy(; programs= "data/cogsci/bridge.json")
    programs = JSON.parsefile(programs)[1:10]

    programs = ["(/seq " * p[2:end] for p in programs]

    corpus = Corpus([Program(parse(SExpr, p), i, i) for (i, p) in enumerate(programs)])

    abstractions, corpus, dfa, _ = compress(corpus; iterations=10, max_arity=2, match_sequences=true)

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

