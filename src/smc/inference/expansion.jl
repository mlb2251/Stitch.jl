function sample_expansion(shared::Shared, abs::Abstraction)::Union{Abstraction, Nothing}
    # if all paths are multiuses, we can't expand
    # all(p -> has_multiuses(p), abs.metavar_paths) && return nothing
    isempty(abs.metavar_paths) && return nothing

    # pick a random match location to use as the basis for expansion
    match = abs.matches[rand(1:end)]

    # pick a random (non-multiused) path to consider expanding
    probs = fill(1., length(abs.metavar_paths))
    for (i, path) in enumerate(abs.metavar_paths)
        # if has_multiuses(path)
        #     probs[i] = 0.
        # end
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
            return multiuse_expansion(shared, abs, match, i, path, child_i, j)
        end
    end

    return syntactic_expansion(shared, abs, match, i, path, child_i)
end

function syntactic_expansion(shared::Shared, abs::Abstraction, match::CorpusNode, i::Int, path_i::MetaVarPath, child_i::CorpusNode)::Abstraction

    # grow the abstraction
    prod = child_i.production
    if prod.type === :app
        args = PExpr[MetaVar(i + j - 1) for j in 1:prod.argc]
        new_expr = App(prod.head, args)
    else
        new_expr = prod.head
    end

    for (k, path) in enumerate(path_i.paths)
        e = k == 1 ? new_expr : copy(new_expr)
        abs.expr = setchild!(abs.expr, path, e)
    end

    # fix the metavar names above i because 1 path will be removed at i and argc paths will be added at i
    shift_by = prod.argc - 1
    if shift_by != 0 # the one case where the insertion and deletion cancel out
        # we haven't yet inserted the new paths or removed the old one,
        # so our old paths that will be kept are just at index i+1 onwards
        for j in i+1:length(abs.metavar_paths)
            path = abs.metavar_paths[j]
            set_indices!(abs.expr, path, j + shift_by)
        end
    end

    hit = true
    new_abs = get!(shared.matches_cache, abs.expr) do
        hit = false

        new_abs = copy(abs)
        new_abs.utility = NaN
        
        # subset to the matches that have the same production as the child we are expanding based on
        filter!(new_abs.matches) do node
            child_i.production_id == getchild(node, path_i).production_id
        end

        # remove the path we're expanding (at index i)
        popat!(new_abs.metavar_paths, i);
        # @assert !has_multiuses(path_i)
        # add the new paths where the old one used to be – at indexes i through i+argc-1
        if prod.type === :app
            for j in 1:prod.argc
                new_paths = Path[copy(path) for path in path_i.paths]
                for path in new_paths
                    push!(path, j)
                end
                insert!(new_abs.metavar_paths, i+j-1, MetaVarPath(new_paths))
            end
        end
        new_abs.size += length(path_i.paths)
        new_abs
    end

    hit!(shared.stats.matches_cache, hit)

    # copy it if it's being used as a key so we don't mess with the key copy
    if !hit
        abs.expr = copy(abs.expr)
    end

    # undo the change to the original expression
    for path in path_i.paths
        abs.expr = setchild!(abs.expr, path, MetaVar(i))
    end

    if shift_by != 0
        for j in i+1:length(abs.metavar_paths)
            path = abs.metavar_paths[j]
            set_indices!(abs.expr, path, j) # undo the shift by setting it back to j
        end
    end

    return new_abs
end

function multiuse_expansion(shared::Shared, abs::Abstraction, match::CorpusNode, i::Int, path_i::MetaVarPath, child_i::CorpusNode, j::Int)
    lo, hi = i < j ? (i,j) : (j,i)

    # set hi to be the same as lo within the expression
    set_indices!(abs.expr, abs.metavar_paths[hi], lo)

    # since we're removing `hi` we need to downshift all the paths above it
    for j in hi+1:length(abs.metavar_paths)
        path = abs.metavar_paths[j]
        set_indices!(abs.expr, path, j - 1)
    end

    hit = true
    new_abs = get!(shared.matches_cache, abs.expr) do
        hit = false

        new_abs = copy(abs)
        new_abs.utility = NaN

        path_lo = new_abs.metavar_paths[lo]
        path_hi = new_abs.metavar_paths[hi]    
        # transfer the hi path and subpaths to now be lo subpaths
        insert_sorted!(path_lo, path_hi.paths)

        # remove hi from the list of paths
        popat!(new_abs.metavar_paths, hi)

        # subset to the matches
        filter!(new_abs.matches) do node
            getchild(node, path_lo).expr_id == getchild(node, path_hi).expr_id
        end
        new_abs
    end

    hit!(shared.stats.matches_cache, hit)

    # copy it since now it might be being used as a key
    if !hit
        abs.expr = copy(abs.expr)
    end

    # undo the changes to the original expression
    set_indices!(abs.expr, abs.metavar_paths[hi], hi)
    for j in hi+1:length(abs.metavar_paths)
        path = abs.metavar_paths[j]
        set_indices!(abs.expr, path, j) # undo the shift by setting it back to j
    end

    return new_abs
end