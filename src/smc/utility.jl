function simple_utility(abs::Abstraction)
    length(abs.matches)*(abs.size - 1 + multiuses(abs)*.9)
end

function utility_by_rewrite(abs::Abstraction)
    # rewritten = rewrite(abs.corpus, abs)
    # rewritten_size = size(rewritten)
    # original_size = size(abs.corpus)
    # return rewritten_size - original_size
    # println(abs)
    # rewritten = rewrite(abs.corpus, abs)

    # original = size(abs.corpus)
    # rw = rewritten_size(abs.corpus, abs)
    # println("rewritten size: $rw, original size: $original")
    return size(abs.corpus) - rewritten_size(abs.corpus, abs)
end

function size_ratio_utility(abs::Abstraction)
    return size(abs.corpus) / rewritten_size(abs.corpus, abs)
end

