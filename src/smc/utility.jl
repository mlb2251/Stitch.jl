function utility(abs::Abstraction)
    length(abs.matches)*(abs.size + abs.multiuses*.9)
end