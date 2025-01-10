
using Stitch
using Test
import JSON

corpus = load_corpus("data/cogsci/dials.json")

first_found_abstraction =  "(C (C (T (T (T l (M 3 (/ pi 2) 0 -2)) (M 1 0 0 0)) (M 1 0 0 0)) (T (T (T l (M 1 0 -0.5 0)) (M 2 0 0 0)) (M 1 0 0 1))) (T (T (T l (M 1 0 -0.5 0)) (M #0 0 0 0)) (M 1 0 0 0)))"
second_found_abstraction = "(M 1 0 #1 #0)"
c_with_hash_zero = "(C (C (T (T (T l (M 3 (/ pi 2) 0 -2)) (M 1 0 0 0)) (M 1 0 0 0)) (T (T (T l (M 1 0 -0.5 0)) (M 2 0 0 0)) (M 1 0 0 1))) #0)"

@testset "follow-partial" begin
    abstractions, _, _, _ = compress(corpus, iterations=1)
    @test string(abstractions[1].body) == first_found_abstraction
    # redundant filter
    abstractions, _, _, _ = compress(corpus, iterations=1, follow=true, track=parse(SExpr, "(C ?? ??)"), silent=true)
    @test string(abstractions[1].body) == first_found_abstraction
    # filter of M ...
    abstractions, _, _, _ = compress(corpus, iterations=1, follow=true, track=parse(SExpr, "(M ?? ?? ?? ??)"), silent=true)
    @test string(abstractions[1].body) == second_found_abstraction
    # filter of (C ?? #0)
    abstractions, _, _, _ = compress(corpus, iterations=1, follow=true, track=parse(SExpr, "(C ?? #0)"), silent=true)
    @test string(abstractions[1].body) == c_with_hash_zero
end
