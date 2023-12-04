# Stitch.jl

```
using Stitch
@time compress(load_corpus("data/cogsci/dials.json"), verbose_best=false)
```

# Concepts

## Matches

A Match is a particular way in which an (potentially partial) abstraction can be applied to a node in a corpus. For example, 
the node in the corpus `(+ (+ 2 3) 5)` can be matched in exactly one way by the abstraction `(+ (+ ?? ??) ??)`.

However, we specifically annotate each node in the corpus with a MatchPossibilities object. This object contains a list of all the ways in which the node can be matched by the abstraction. This comes up in cases such as sequences. For example, the node in the corpus `(seq a b b)` can be matched by the
abstraction `(seq a ?0 b ??...)` in two ways, where `?0` is either `Just b` or `Nothing` and the hole is either `(seq)` or `(seq b)`, respectively.

We can conceptualize this as a Set of Matches, which leads to a Monad structure, as we will discuss later.

## Expansion

An expansion is a particular way in which an abstraction can be expanded to replace a hole. For example, the abstraction `(+ ?? 2)` can be expanded to `(+ 3 2)`, or to `(+ (+ ?? ??) 2)`, `(+ $0 2)`, etc. We conceptualize expansions as having the following interface (using Haskell notation for clarity)

### Expansion Interface (Haskell)

```haskell
class Expansion e where
    -- Take a list of tagged matches, and return a list of expansions along with the matches that the expansion applies to
    collect :: Abstraction -> [(i, Match)] -> [(e, [(i, Match)])]

    -- mutate the abstraction to apply the expansion, and to unapply the expansion
    expand_abstraction :: e -> State AbstractionInfo ()
    unexpand_abstraction :: e -> State AbstractionInfo ()

    -- produce a list of matches that result from applying the expansion to a particular match
    -- potentially mutating the match in the process
    expand_match :: e -> State Match [Match]
    -- unapply the mutations performed by expand_match
    unexpand_match :: e -> State Match ()

    -- compute the utility of the expansion on a particular match
    delta_local_utility :: e -> Match -> Float
```

where AbstractionInfo contains the Abstraction object along with the holes being tracked.

These commands can then be used to implement the following functions:

```haskell
collect_multi :: (Expansion e) => e -> [MatchPossibilities] -> [(e, [MatchPossibilities])]

expand_match_multi :: (Expansion e) => e -> State MatchPossibilities MatchPossibilities
unexpand_match_multi :: (Expansion e) => e -> State MatchPossibilities ()

update_local_utility :: (Expansion e) => e -> State MatchPossibilities ()
```

### Expansion Interface (Julia)

```julia
# corresponds to collect in Haskell
collect_expansions(
    ::Type{Expansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)
```

We implement these 