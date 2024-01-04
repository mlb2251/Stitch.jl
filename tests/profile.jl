
using Stitch
using Test
using Statistics
using JSON
using Profile
using FlameGraphs
using Images

include("./framework.jl")

run(s) = integrate(s, "$s-out.json")

function fn_to_run()
    oldstd = stdout
    redirect_stdout(devnull)
    t = @elapsed run("data/imperative_realistic/2.json")
    redirect_stdout(oldstd) # recover original stdout
    println(t)
    t
end

function main()
    fn_to_run()
    @profview fn_to_run()
end

main()