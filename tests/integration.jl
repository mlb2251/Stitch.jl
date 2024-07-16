
using Stitch
using Test
import JSON

include("./framework.jl")
run(s) = integrate(s, "$s-out.json")

is_testing = true

# full_tests()
run("data/imperative/subsequence-repeated-adjacents-even.json")