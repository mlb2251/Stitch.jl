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
    return rewritten_size(abs.corpus, abs) - size(abs.corpus)
end

function top_down_utility(corpus::Corpus, abs::Abstraction)

end
