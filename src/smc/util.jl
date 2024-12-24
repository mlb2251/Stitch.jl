"""
adapted from StatsBase.sample();
this verison requires normalized weights (uncomment the first line sum to work with unnormalized weights)
"""
function sample_normalized(weights)
    t = rand() # * sum(w -> w, weights) 
    n = length(weights)
    i = 1
    @inbounds cw = weights[1]
    while cw < t && i < n
        i += 1
        @inbounds cw += weights[i]
    end
    return i
end

const Id = Int

struct IdSet{T}
    id_of_entry::Dict{T, Id}
    entry_of_id::Vector{T}
end
IdSet{T}() where T = IdSet(Dict{T, Id}(), Vector{T}())
function Base.getindex(idset::IdSet{T}, entry::T) where T
    get!(idset.id_of_entry, entry) do
        push!(idset.entry_of_id, entry)
        length(idset.entry_of_id)
    end
end
function Base.getindex(idset::IdSet{T}, id::Id) where T
    idset.entry_of_id[id]
end