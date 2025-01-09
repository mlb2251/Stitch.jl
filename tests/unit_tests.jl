
using Stitch
using Test
import JSON

corpus = load_corpus("data/cogsci/dials.json")

found_abstractions = [
    "(C (C (T (T (T l (M 3 (/ pi 2) 0 -2)) (M 1 0 0 0)) (M 1 0 0 0)) (T (T (T l (M 1 0 -0.5 0)) (M 2 0 0 0)) (M 1 0 0 1))) (T (T (T l (M 1 0 -0.5 0)) (M #0 0 0 0)) (M 1 0 0 0)))",
    "(M 1 0 #1 #0)",
    "(C #1 (T (T (T l (fn_2 0 -0.5)) (M #0 (/ pi 4) 0 0)) (fn_2 (* #0 (* 0.5 (sin (/ pi 4)))) (* #0 (* 0.5 (cos (/ pi 4)))))))",
]

@testset "follow-partial" begin
    abstractions, _, _, _ = compress(corpus, iterations=1)
    @test string(abstractions[1].body) == found_abstractions[1]
    abstractions, _, _, _ = compress(corpus, iterations=1, follow=true, track=parse(SExpr, "(M ?? ?? ?? ??)"), silent=true)
    @test string(abstractions[1].body) == found_abstractions[2]
end
