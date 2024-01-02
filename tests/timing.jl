
using Stitch
using Test
import JSON

include("./framework.jl")

is_testing = false
strict = false

run(s) = integrate(s, "$s-out.json")

function fn_to_run()
    oldstd = stdout
    redirect_stdout(devnull)
    full_tests()
    redirect_stdout(oldstd) # recover original stdout
end

function main()
    fn_to_run()
    times = []
    for x in 1:3
        t = @elapsed fn_to_run()
        push!(times, t)
    end
    println("Individual times: ", [x for x in times], ". Mean: ", sum(times) / length(times))
end

main()