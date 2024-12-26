function utility(abs::Abstraction)
    length(abs.matches)*(abs.size + abs.multiuses*.9)
end


function top_down_utility(corpus::Corpus, abs::Abstraction)
    
end
