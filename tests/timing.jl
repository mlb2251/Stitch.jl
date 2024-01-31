
using Stitch
using Test
using Statistics
using JSON
using ArgParse

include("./framework.jl")

run(s) = integrate(s, "$s-out.json")

s = ArgParseSettings()
@add_arg_table s begin
    "--folders"
    help = "Folders to use"
    nargs = '+'
end

@add_arg_table s begin
    "--paths"
    help = "Paths to use"
    nargs = '+'
end

args = parse_args(s)

function tests()
    if length(args["folders"]) + length(args["paths"]) != 0
        [folder_tests(folder) for folder in args["folders"]]
        [run(path) for path in args["paths"]]
    else
        full_tests()
    end
end

function fn_to_run()
    oldstd = stdout
    redirect_stdout(devnull)
    t = @elapsed tests()
    redirect_stdout(oldstd) # recover original stdout
    # print t and then flush
    println(t)
    flush(stdout)
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