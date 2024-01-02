
using Stitch
using Test
using Statistics
using JSON

include("./framework.jl")

function fn_to_run()
    oldstd = stdout
    redirect_stdout(devnull)
    t = @elapsed full_tests()
    redirect_stdout(oldstd) # recover original stdout
    println(t)
    t
end

function main()
    fn_to_run()
    times = []
    for x in 1:5
        t = fn_to_run()
        push!(times, t)
    end
    println("Individual times: ", [round(x, digits=2) for x in times], ". Median: ", round(Statistics.median(times), digits=2))
end

main()