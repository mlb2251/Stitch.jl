function sample_expansion!(shared::Shared, abs::Abstraction)
    all(p -> isfrozen(p), abs.metavar_paths) && return false

    # pick a random match location to use as the basis for expansion
    match = abs.matches[rand(1:end)]

    # pick a random (unfrozen) path to consider expanding
    probs = fill(1., length(abs.metavar_paths))
    for (i, path) in enumerate(abs.metavar_paths)
        if isfrozen(path)
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
            j != i && child_i.expr_id == getchild(match, abs.metavar_paths[j]).expr_id
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

    # grow the abstraction
    prod = child_i.production
    if prod.type === :app
        new_expr = App(prod.head, PExpr[])
        for j in 1:prod.argc
            push!(new_expr.args, MetaVar(length(abs.metavar_paths) + 1))
        end
    else
        new_expr = prod.head
    end
    abs.expr = setchild!(abs.expr, path_i, new_expr)

    # fix the metavar names above i because 1 path will be removed at i and argc paths will be added at i
    shift_by = prod.argc - 1
    if shift_by != 0 # the one case where the insertion and deletion cancel out
        # we haven't yet inserted the new paths or removed the old one,
        # so our old paths that will be kept are just at index i+1 onwards
        for j in i+1:length(abs.metavar_paths)
            path = abs.metavar_paths[j]
            setchild!(abs.expr, path, MetaVar(j + shift_by))
            for subpath in path.subpaths
                setchild!(abs.expr, subpath, MetaVar(j + shift_by))
            end
        end
    end




    # remove the path we're expanding (at index i)
    popat!(abs.metavar_paths, i);

    # add the new paths where the old one used to be – at indexes i through i+argc-1
    if prod.type === :app
        for j in 1:prod.argc
            new_path = MetaVarPath(copy(path_i.path), Path[])
            push!(new_path.path, j)
            insert!(abs.metavar_paths, i+j-1, new_path)
        end
    end


    abs.size += 1

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
    lo, hi = i < j ? (i,j) : (j,i)
    path_lo = abs.metavar_paths[lo]
    path_hi = abs.metavar_paths[hi]

    # set hi to be the same as lo within the expression
    abs.expr = setchild!(abs.expr, path_hi.path, MetaVar(lo))
    for subpath in path_hi.subpaths
        abs.expr = setchild!(abs.expr, subpath, MetaVar(lo))
    end

    # since we're removing `hi` we need to downshift all the paths above it
    for j in hi+1:length(abs.metavar_paths)
        path = abs.metavar_paths[j]
        setchild!(abs.expr, path, MetaVar(j - 1))
        for subpath in path.subpaths
            setchild!(abs.expr, subpath, MetaVar(j - 1))
        end
    end

    # transfer the hi path and subpaths to now be lo subpaths
    push!(path_lo.subpaths, path_hi.path)
    append!(path_lo.subpaths, path_hi.subpaths)

    # remove hi from the list of paths
    popat!(abs.metavar_paths, hi)



    # subset to the matches
    hit!(shared.stats.matches_cache)
    abs.matches = get!(shared.matches_cache, abs.expr) do
        unhit!(shared.stats.matches_cache) # silly
        miss!(shared.stats.matches_cache)
        filter!(abs.matches) do node
            getchild(node, path_lo).expr_id == getchild(node, path_hi).expr_id
        end
    end

    abs.utility = simple_utility(abs)
end