function sample_expansion!(shared::Shared, abs::Abstraction)
    all(p -> p.frozen, abs.metavar_paths) && return false

    # pick a random match location to use as the basis for expansion
    match = abs.matches[rand(1:end)]

    # pick a random (unfrozen) path to consider expanding
    probs = fill(1., length(abs.metavar_paths))
    for (i, path) in enumerate(abs.metavar_paths)
        if path.frozen
            probs[i] = 0.
        end
    end
    probs ./= sum(probs)

    i = sample_normalized(probs)
    path = abs.metavar_paths[i]
    child_i = getchild(match, path)

    shared.stats.expansions += 1

    P_MULTIUSE = 0.5

    if rand() < P_MULTIUSE
        # consider multiuse expansion – is there another argument that is the same as the one we are expanding (at this location)?
        # note due to freezing we can't modify the other path, but we can modify ourselves to be the same as the other path
        multiuse_candidates = filter(eachindex(abs.metavar_paths)) do j
            j != i && abs.metavar_paths[i].name != abs.metavar_paths[j].name && child_i.expr_id == getchild(match, abs.metavar_paths[j]).expr_id
        end

        if length(multiuse_candidates) > 0
            # pick a random one
            j = multiuse_candidates[rand(1:end)]
            multiuse_expansion!(shared, abs, match, i, path, child_i, j)
            return true
        end
    end

    syntactic_expansion!(shared, abs, match, i, path, child_i)
    return true
end

function syntactic_expansion!(shared::Shared, abs::Abstraction, match::CorpusNode, i::Int, path_i::MetaVarPath, child_i::CorpusNode)

    popat!(abs.metavar_paths, i);

    # grow the abstraction
    prod = child_i.production
    if prod.type === :app
        new_expr = App(prod.head, PExpr[])
        for j in 1:prod.argc
            push!(new_expr.args, MetaVar(abs.fresh_metavar))
            new_path = MetaVarPath(copy(path_i.path), abs.fresh_metavar, false, true)
            push!(new_path.path, j)
            push!(abs.metavar_paths, new_path)
            abs.fresh_metavar += 1
            abs.arity += 1
        end
    else
        new_expr = prod.head
    end
    abs.expr = setchild!(abs.expr, path_i, new_expr)
    abs.size += 1
    abs.arity -= 1

    hit!(shared.stats.matches_cache)
    # subset to the matches that have the same production as the child we are expanding based on
    abs.matches = get!(shared.matches_cache, abs.expr) do
        unhit!(shared.stats.matches_cache) # silly
        miss!(shared.stats.matches_cache)
        filter!(abs.matches) do node
            child_i.production_id == getchild(node, path_i).production_id
        end
    end

    abs.utility = simple_utility(abs)
end

function multiuse_expansion!(shared::Shared, abs::Abstraction, match::CorpusNode, i::Int, path_i::MetaVarPath, child_i::CorpusNode, j::Int)
    path_j = abs.metavar_paths[j]

    # path_j = abs.metavar_paths[j]
    abs.metavar_paths[i].frozen = true
    abs.metavar_paths[j].frozen = true
    # set i to be the same as j but not as a representative (and note j may or may not be a representative)
    abs.metavar_paths[i].name = path_j.name
    abs.metavar_paths[i].representative = false

    # set the two vars to be the same. `j` is the one that will be kept since it might already be frozen
    abs.expr = setchild!(abs.expr, path_i, MetaVar(path_j.name))
    abs.multiuses += 1
    abs.arity -= 1

    # subset to the matches
    hit!(shared.stats.matches_cache)
    abs.matches = get!(shared.matches_cache, abs.expr) do
        unhit!(shared.stats.matches_cache) # silly
        miss!(shared.stats.matches_cache)
        filter!(abs.matches) do node
            getchild(node, path_i).expr_id == getchild(node, path_j).expr_id
        end
    end

    abs.utility = simple_utility(abs)
end