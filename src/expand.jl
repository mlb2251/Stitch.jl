using Random

function possible_expansions!(search_state)
    isempty(search_state.expansions) || error("expansions should be empty")

    expansions!(SyntacticLeafExpansion, search_state)
    expansions!(AbstractionExpansion, search_state)
    expansions!(SymbolExpansion, search_state)
    if search_state.config.match_sequences
        expansions!(SequenceExpansion, search_state)
        expansions!(SequenceElementExpansion, search_state)
        expansions!(SequenceTerminatorExpansion, search_state)
        expansions!(SequenceChoiceVarExpansion, search_state)
    end

    if search_state.config.shuffle_expansions_seed !== nothing
        rng = MersenneTwister(search_state.config.shuffle_expansions_seed)
        shuffle!(rng, search_state.expansions)
    end

    # sort!(search_state.expansions, by=e -> length(e.matches)*e.matches[1].local_utility)
    # sort!(search_state.expansions, by=e -> upper_bound_fn(search_state,e))

    # if tracking is turned on we'll defer the pruning till later when we can more easily indicate things being pruned
    !isnothing(search_state.config.track) && return

    # filter out ones that dont pass bounds check
    # filter!(e -> upper_bound_fn(search_state,e) > best_util, search_state.expansions);
end

function expansions!(typ::Type{T}, search_state::SearchState{Match}) where T # force specialization
    flattened_matches = [(i, m) for (i, m) in enumerate(search_state.matches)]
    search_state.stats.matches_considered += length(flattened_matches)
    res = collect_expansions(typ, search_state.abstraction, flattened_matches, search_state.config)
    for (expansion, tagged_matches) in res
        push!(search_state.expansions, PossibleExpansion(
            [m for (_, m) in tagged_matches],
            expansion,
        ))
    end
end

function expansions!(typ::Type{T}, search_state::SearchState{MatchPossibilities}) where T
    flattened_matches = Vector{Tuple{Int,Match}}()
    for (i, match_poss) in enumerate(search_state.matches)
        for match in match_poss.alternatives
            push!(flattened_matches, (i, match))
        end
    end
    search_state.stats.matches_considered += length(flattened_matches)
    res = collect_expansions(typ, search_state.abstraction, flattened_matches, search_state.config)
    for (expansion, tagged_matches) in res
        out = Dict{Int,Vector{Match}}()
        ks = Vector{Int}()
        for (i, match) in tagged_matches
            if !(i in keys(out))
                push!(ks, i)
            end
            push!(get!(()->Match[], out, i), match)
        end
        poss = Vector{MatchPossibilities}()
        for i in ks
            push!(poss, MatchPossibilities(out[i]))
        end
        push!(search_state.expansions, PossibleExpansion(
            poss,
            expansion,
        ))
    end
end


"""
Adds the set of expansions to whatever terminal or nonterminal is present at the match locations,
for example primitives or variables.
"""
function collect_expansions(
    ::Type{SyntacticLeafExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}}, config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}
    matches_of_leaf = Dict{Symbol,Vector{Tuple{Int,Match}}}() # can't prealloc - these must be fresh array objects that must persist and cant be cleared after this!
    matches_of_node = Dict{Tuple{Symbol,Int},Vector{Tuple{Int,Match}}}()
    for (i, match) in matches
        if !(match.holes[end] isa TreeNodeHole)
            continue
        end
        if is_leaf(match.holes[end])
            # leaf case
            leaf = match.holes[end].leaf
            startswith(string(leaf), "&") && continue

            matches_for_leaf = get!(matches_of_leaf, leaf) do
                Match[]
            end

            push!(matches_for_leaf, (i, match))
        else
            # node case - group with other nodes that have same number of children (and head if autoexpand_head is on)
            head = match.holes[end].children[1].leaf

            if head === SYM_SEQ_HEAD # this is a sequence
                continue
            end

            if !config.autoexpand_head
                head = :no_expand_head
            end

            childcount = length(match.holes[end].children)
            matches_for_leaf = get!(matches_of_node, (head, childcount)) do
                Match[]
            end
            push!(matches_for_leaf, (i, match))
        end
    end

    result = Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}()

    for (leaf, matches) in matches_of_leaf
        isempty(matches) && continue
        push!(result, (SyntacticLeafExpansion(leaf), matches))
    end
    for ((head, childcount), matches) in matches_of_node
        isempty(matches) && continue
        push!(result, (SyntacticNodeExpansion(head, childcount), matches))
    end

    result
end

function collect_expansions(
    ::Type{SymbolExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}}, config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}
    if abstraction.body.leaf === SYM_HOLE
        # Never a valid abstraction to just have a single symvar.
        # Excluded deliberately because it breaks our assumption
        # that we can assume that symbols in the same context
        # will always have the same root state.
        return []
    end
    matches_of_idx = Dict{Int,Vector{Tuple{Int,Match}}}()
    freshness_of_idx = Dict{Int,Bool}()
    sym_of_idx = Dict{Int,Symbol}()
    for (i, match) in matches
        if !(match.holes[end] isa TreeNodeHole)
            continue
        end
        is_leaf(match.holes[end]) || continue
        sym = match.holes[end].leaf
        if !startswith(string(sym), "&") # this is not a symbol
            continue
        end
        fresh = !haskey(match.idx_of_sym, sym)
        idx = get(match.idx_of_sym, sym, length(match.sym_of_idx) + 1)
        ms = get!(matches_of_idx, idx) do
            Vector{Tuple{Int,Match}}()
        end
        if !haskey(freshness_of_idx, idx)
            freshness_of_idx[idx] = fresh
            sym_of_idx[idx] = match.holes[end].metadata.dfa_state
        else
            @assert freshness_of_idx[idx] == fresh
            @assert sym_of_idx[idx] == match.holes[end].metadata.dfa_state
        end
        push!(ms, (i, match))
    end

    result = Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}()

    for (idx, matches) in matches_of_idx
        if isempty(matches)
            continue
        end
        push!(result, (SymbolExpansion(idx, freshness_of_idx[idx], sym_of_idx[idx]), matches))
    end

    result
end

function collect_expansions(
    ::Type{AbstractionExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    matches = filter(((_, m),) -> m.holes[end] isa TreeNodeHole, matches)

    result = Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}()

    function collect_abstraction_expansions_for_dfa_state!(ms, sym)

        if length(ms) == 0
            return
        end
        # variable reuse
        for i in 0:abstraction.arity-1
            ms_specific = filter(((_, m),) -> m.holes[end].metadata.struct_hash == m.unique_args[i+1].metadata.struct_hash, ms)
            # ms_specific = [m for m in ms_specific if m.holes[end].metadata.struct_hash == m.unique_args[i+1].metadata.struct_hash]
            if isempty(ms_specific)
                continue
            end

            push!(result, (AbstractionExpansion(i, false, sym), ms_specific))
        end

        if can_accept_metavar(abstraction, config)
            # fresh variable
            push!(result, (AbstractionExpansion(abstraction.arity, true, sym), ms))
        end

    end

    if isnothing(config.dfa)
        collect_abstraction_expansions_for_dfa_state!(matches, :uninit_state)
    else
        matches_for_state = Dict{Symbol, Vector{Tuple{Int,Match}}}()
        for (i, match) in matches
            hole = match.holes[end]
            if !(hole isa TreeNodeHole)
                continue
            end
            dfa_state = hole.metadata.dfa_state
            allowed = (
                dfa_state === :E
                || (config.dfa_metavariable_allow_S && dfa_state === :S)
                || (config.dfa_metavariable_allow_seqS && dfa_state === :seqS)
                || config.dfa_metavariable_allow_anything
            )
            if allowed
                matches_for_this_state = get!(matches_for_state, dfa_state) do
                    Vector{Tuple{Int,Match}}()
                end
                push!(matches_for_this_state, (i, match))
            end
        end
        for (state, ms) in matches_for_state
            collect_abstraction_expansions_for_dfa_state!(ms, state)
        end
    end
    result
end

function collect_expansions(
    ::Type{SequenceExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    matches = filter(matches) do (i, match)
        if !(match.holes[end] isa TreeNodeHole)
            return false
        end
        !is_leaf(match.holes[end]) && match.holes[end].children[1].leaf === SYM_SEQ_HEAD
    end
    if length(matches) == 0
        return []
    end

    # currently only executed when abstraction is ??. Means this is effectively the first expansion
    is_root = is_leaf(abstraction.body)

    if is_root
        # in this context, we expand to a subsequence node as well as a sequence node.
        return [(SequenceExpansion(true), matches), (SequenceExpansion(false), matches)]
    else
        return [(SequenceExpansion(false), matches)]
    end
end

function collect_expansions(
    ::Type{SequenceElementExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    matches = filter(matches) do (_, match)
        hole = match.holes[end]
        if !(hole isa RemainingSequenceHole)
            return false
        end
        hole.num_consumed < length(hole.root_node.children)
    end
    if length(matches) == 0
        return []
    end
    return [(SequenceElementExpansion(), matches)]
end

function collect_expansions(
    ::Type{SequenceTerminatorExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    matches_by_subseq = [Vector{Tuple{Int,Match}}(), Vector{Tuple{Int,Match}}()]

    for (tag, match) in matches
        hole = match.holes[end]
        if !(hole isa RemainingSequenceHole)
            continue
        end
        if hole.is_subseq
            # if the hole is a subseq we can terminate whenever
            push!(matches_by_subseq[2], (tag, match))
        else
            # if the whole is a full sequence, we need to make sure we've consumed all the elements first
            if hole.num_consumed == length(hole.root_node.children)
                push!(matches_by_subseq[1], (tag, match))
            end
        end
    end
    result = Tuple{Expansion,Vector{Tuple{Int,Match}}}[]
    if length(matches_by_subseq[1]) > 0
        push!(result, (SequenceTerminatorExpansion(false), matches_by_subseq[1]))
    end
    if length(matches_by_subseq[2]) > 0
        push!(result, (SequenceTerminatorExpansion(true), matches_by_subseq[2]))
    end
    return result
end

function collect_expansions(
    ::Type{SequenceChoiceVarExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    if !can_accept_choicevar(abstraction, config)
        return []
    end

    matches_by_sym = Dict{Symbol,Vector{Tuple{Int,Match}}}()

    for (tag, match) in matches
        hole = match.holes[end]
        if !(hole isa RemainingSequenceHole)
            continue
        end
        sym = hole.root_node.metadata.seq_element_dfa_state
        if hole.num_consumed <= length(hole.root_node.children)
            v = get!(()->Tuple{Int,Match}[], matches_by_sym, sym)
            push!(v, (tag, match))
        end
    end

    return [(SequenceChoiceVarExpansion(abstraction.choice_arity, k), m) for (k, m) in matches_by_sym]
end

"""
Saves current state to the stack, and reinitializes as a fresh state
"""
function expand_general!(search_state, expansion)

    # pop hole
    hole = pop!(search_state.holes)
    # hole_dfa_state = pop!(search_state.hole_dfa_states)
    is_hole(hole) || is_seq_hole(hole) || error("not a hole: $hole")
    push!(search_state.holes_stack, hole)
    # push!(search_state.hole_dfa_states_stack,hole_dfa_state)

    # save state for backtracking
    push!(search_state.expansions_stack, search_state.expansions)
    push!(search_state.matches_stack, search_state.matches)
    push!(search_state.past_expansions, expansion)

    # fresh .expansions
    search_state.expansions = PossibleExpansion[]
    # set .matches properly
    search_state.matches = expansion.matches

    # expand the state
    expand!(search_state, expansion.data, hole)

    expand_utilities!(expansion.data, search_state)

    check_number_of_holes(search_state)
end

function expand_utilities!(data, search_state::SearchState{Match})
    for match in search_state.matches
        # save the local utility for backtracking
        push!(match.local_utility_stack, match.local_utility)
        match.local_utility += delta_local_utility(search_state.config, match, data)
    end
end

function expand_utilities!(data, search_state::SearchState{MatchPossibilities})
    for match_poss in search_state.matches
        for match in match_poss.alternatives
            # save the local utility for backtracking
            push!(match.local_utility_stack, match.local_utility)
            match.local_utility += delta_local_utility(search_state.config, match, data)
        end
    end
end

function unexpand_general!(search_state::SearchState)

    hole = pop!(search_state.holes_stack)
    # hole_dfa_state = pop!(search_state.hole_dfa_states_stack);

    # pop the expansion to undo
    expansion = pop!(search_state.past_expansions)

    unexpand_utilities!(search_state)

    unexpand!(search_state, expansion.data, hole)

    # unexpand - this should be an inverse to expand!()
    search_state.matches === expansion.matches || error("mismatched matches")

    # restore other state
    search_state.expansions = pop!(search_state.expansions_stack)
    search_state.matches = pop!(search_state.matches_stack)
    push!(search_state.holes, hole)
    # push!(search_state.hole_dfa_states, hole_dfa_state)

    check_number_of_holes(search_state)
    # all(match -> length(match.args) == length(search_state.args), search_state.matches) || error("mismatched number of holes")    

end

function unexpand_utilities!(search_state::SearchState{Match})
    for match in search_state.matches
        match.local_utility = pop!(match.local_utility_stack)
    end
end

function unexpand_utilities!(search_state::SearchState{MatchPossibilities})
    for match_poss in search_state.matches
        for match in match_poss.alternatives
            match.local_utility = pop!(match.local_utility_stack)
        end
    end
end

function has_number_of_holes(match::Match, num_holes)
    length(match.holes) == num_holes
end

function has_number_of_holes(match_poss::MatchPossibilities, num_holes)
    all(m -> has_number_of_holes(m, num_holes), match_poss.alternatives)
end

function check_number_of_holes(search_state)
    all(m -> has_number_of_holes(m, length(search_state.holes)), search_state.matches) || error("mismatched number of holes")
end

function expand!(search_state::SearchState{Match}, expansion, hole)
    expand_abstraction!(expansion, hole, search_state.holes, search_state.abstraction, search_state.config)
    for match in search_state.matches
        extras = expand_match!(search_state.config, expansion, match)
        @assert extras === nothing
    end
end

function expand!(search_state::SearchState{MatchPossibilities}, expansion, hole)

    expand_abstraction!(expansion, hole, search_state.holes, search_state.abstraction, search_state.config)
    new_match_poss = MatchPossibilities[]
    whole_list_update = false
    for (idx, match_poss) in enumerate(search_state.matches)
        updated_matches = Match[]
        match_poss_update = false
        for (alt_idx, match) in enumerate(match_poss.alternatives)
            extras = expand_match!(search_state.config, expansion, match)
            push!(updated_matches, match)
            if extras === nothing
                continue
            else
                if !match_poss_update
                    match_poss_update = true
                    append!(updated_matches, match_poss.alternatives[1:alt_idx-1])
                end
                append!(updated_matches, extras)
            end
        end
        if !match_poss_update
            updated_matches = match_poss.alternatives
        end
        new_matches = collapse_equivalent_matches(expansion, updated_matches)
        if !isnothing(new_matches)
            match_poss_update = true
            updated_matches = new_matches
        end
        if match_poss_update
            if !whole_list_update
                whole_list_update = true
                new_match_poss = MatchPossibilities[]
                append!(new_match_poss, search_state.matches[1:idx-1])
            end
            match_poss = MatchPossibilities(updated_matches)
            push!(new_match_poss, match_poss)
        else
            if whole_list_update
                push!(new_match_poss, match_poss)
            end
        end
    end
    if !whole_list_update
        new_match_poss = search_state.matches
    end
    push!(search_state.matches_stack, search_state.matches)
    search_state.matches = new_match_poss
end

function expand_abstraction!(expansion::SyntacticLeafExpansion, hole, holes, abstraction, config::SearchConfig)
    # set the head symbol of the hole
    hole.leaf = expansion.leaf
    abstraction.body_size += symbol_size(expansion.leaf, config.size_by_symbol)
end

function expand_match!(config::SearchConfig, expansion::SyntacticLeafExpansion, match::Match)::Nothing
    hole = pop!(match.holes)::TreeNodeHole
    match.holes_size -= hole.metadata.size
    push!(match.holes_stack, hole)
    @assert is_leaf(hole)
    return nothing
end

function expand_abstraction!(expansion::SyntacticNodeExpansion, hole, holes, abstraction, config::SearchConfig)
    # make it no longer a leaf
    hole.leaf = nothing

    # add fresh holes to .args and also keep track of them in search_state.abstraction.holes
    for i in 1:expansion.num_holes
        h = new_hole((hole, i))
        push!(hole.children, h)
        if i == 1 && expansion.head !== :no_expand_head
            # set the head symbol of the hole and dont push it to the list of search state holes
            h.leaf = expansion.head
        else
            push!(holes, h)
            # state = if expansion.head === :no_expand_head || isnothing(search_state.config.dfa)
            #     :no_dfa_state
            # else
            #     @show i
            #     @show dfa_state
            #     @show expansion.head
            #     @show search_state.config.dfa[dfa_state][expansion.head]
            #     search_state.config.dfa[dfa_state][expansion.head][i-1]
            # end
            # push!(search_state.hole_dfa_states, state)
        end
    end

    # reverse holes so they go left to right
    # @views reverse!(search_state.holes[end-expansion.num_holes+1:end]) 
    if expansion.head !== :no_expand_head
        abstraction.body_size += symbol_size(expansion.head, config.size_by_symbol)
    end
end

function expand_match!(config::SearchConfig, expansion::SyntacticNodeExpansion, match::Match)::Nothing
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)::TreeNodeHole
    length(hole.children) == expansion.num_holes || error("mismatched number of children to expand to at location: $(match.expr) with hole $hole for expansion $(expansion)")
    push!(match.holes_stack, hole)

    # add all the children of the hole as new holes (except possibly the head)
    if expansion.head !== :no_expand_head
        append!(match.holes, hole.children[2:end])
        match.holes_size -= hole.children[1].metadata.size
    else
        append!(match.holes, hole.children)
    end
    return nothing
end


function expand_abstraction!(expansion::AbstractionExpansion, hole, holes, abstraction, config::SearchConfig)
    hole.leaf = Symbol("#$(expansion.index)")

    if expansion.fresh
        abstraction.arity += 1
        push!(abstraction.dfa_metavars, expansion.dfa_state)
    end

    abstraction.body_size += 1
end

function expand_match!(config::SearchConfig, expansion::AbstractionExpansion, match::Match)::Nothing
    hole = pop!(match.holes)::TreeNodeHole
    match.holes_size -= hole.metadata.size
    push!(match.holes_stack, hole)
    if expansion.fresh
        push!(match.unique_args, hole) # move the hole to be an argument
    end
    return nothing
end

function expand_abstraction!(expansion::SymbolExpansion, hole, holes, abstraction, config::SearchConfig)
    # set the head symbol of the hole
    hole.leaf = Symbol("%$(expansion.idx)")

    if expansion.fresh
        abstraction.sym_arity += 1
        push!(abstraction.dfa_symvars, expansion.dfa_state)
    end

    abstraction.body_size += 1
end

function expand_match!(config::SearchConfig, expansion::SymbolExpansion, match::Match)::Nothing
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)::TreeNodeHole
    match.holes_size -= hole.metadata.size
    push!(match.holes_stack, hole)

    @assert string(hole.leaf)[1] == '&'

    if expansion.fresh
        # this is a new symbol
        push!(match.sym_of_idx, hole.leaf)
        match.idx_of_sym[hole.leaf] = expansion.idx
    end
    return nothing
end


function expand_abstraction!(expansion::SequenceExpansion, hole, holes, abstraction, config::SearchConfig)
    # take a hole ?? and make it (/seq ...). The hole is then pushed to the stack
    hole.leaf = nothing
    head = new_hole((hole, 1))
    head.leaf = if expansion.is_subseq
        SYM_SUBSEQ_HEAD
    else
        SYM_SEQ_HEAD
    end
    nh = new_seq_hole((hole, 2))

    push!(hole.children, head)
    push!(hole.children, nh)

    push!(holes, hole)

    abstraction.body_size += symbol_size(head.leaf, config.size_by_symbol)
end

function expand_match!(config::SearchConfig, expansion::SequenceExpansion, match::Match)::Union{Nothing,Vector{Match}}
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)::TreeNodeHole
    push!(match.holes_stack, hole)
    if expansion.is_subseq
        match_main = match
        match = copy_match(match)
    else
        match_main = match
    end
    # add a hole representing the remaining sequence
    push!(match_main.holes, RemainingSequenceHole(hole, 1, expansion.is_subseq))
    match_main.holes_size -= hole.children[1].metadata.size # remove the /seq node
    if !expansion.is_subseq
        return nothing
    end
    matches = Match[]
    # start can consume any number of elements. We've already added the case where it consumes 0 elements
    for start_consumes in 1:length(hole.children)-1
        # add a hole representing the remaining sequence
        match_copy = copy_match(match)
        match_copy.start_items = start_consumes + 1
        add_remaining_sequence_hole(match_copy, hole, expansion)
        push!(matches, match_copy)
    end
    matches
end

function add_remaining_sequence_hole(match_copy::Match, hole::SExpr, expansion::SequenceExpansion)
    start_consumes = if match_copy.start_items === nothing
        1
    else
        match_copy.start_items
    end
    push!(match_copy.holes, RemainingSequenceHole(hole, start_consumes, expansion.is_subseq))
    for i in 1:start_consumes
        match_copy.holes_size -= hole.children[i].metadata.size
    end
end

function insert_before_sequence_hole!(create_new, hole, holes)
    # take a hole (/seq <things> ...) and make it (/seq <things> <new> ...). Also manipulate the stack of holes,
    # so that the ... hole is updated for the fact that it is now one further to the right. It is not
    # removed because it can still be filled in with more elements.

    # The newly created hole is then returned

    i = length(hole.children)
    new_element = create_new(i)
    # overwrite the old ...
    hole.children[i] = new_element

    new_sequence_hole = new_seq_hole((hole, i + 1))
    push!(hole.children, new_sequence_hole)

    push!(holes, hole)
    new_element
end

function expand_abstraction!(expansion::SequenceElementExpansion, hole, holes, abstraction, config::SearchConfig)
    element_hole = insert_before_sequence_hole!(i -> new_hole((hole, i)), hole, holes)
    push!(holes, element_hole)

    # no change to abstraction.body_size since we are just replacing a ... with a ?? ...
end

function expand_match!(config::SearchConfig, expansion::SequenceElementExpansion, match::Match)::Nothing
    last_hole = pop!(match.holes)
    @assert last_hole isa RemainingSequenceHole
    # push the hole back on the stack
    push!(match.holes_stack, last_hole)

    new_sequence_hole = RemainingSequenceHole(last_hole.root_node, last_hole.num_consumed + 1, last_hole.is_subseq)
    push!(match.holes, new_sequence_hole)
    push!(match.holes, new_sequence_hole.root_node.children[new_sequence_hole.num_consumed])

    # this does not affect holes_size since we are just replacing a ... with a ?? and a ...
    return nothing
end

function expand_abstraction!(expansion::SequenceTerminatorExpansion, hole, holes, abstraction, config::SearchConfig)
    # just remove the last hole, and implicitly close off the sequence
    pop!(hole.children)

    # no change to abstraction.body_size since we are just removing a ...
end

function expand_match!(config::SearchConfig, expansion::SequenceTerminatorExpansion, match::Match)::Nothing
    # pop next hole and save it for future backtracking
    last_hole = pop!(match.holes)
    @assert last_hole isa RemainingSequenceHole
    @assert expansion.is_subseq || last_hole.num_consumed == length(last_hole.root_node.children)
    push!(match.holes_stack, last_hole)
    # this does not affect holes_size since we are just removing a ... that currently matches nothing
    if expansion.is_subseq
        match.end_items = last_hole.num_consumed
        for i in match.end_items+1:length(last_hole.root_node.children)
            match.holes_size -= last_hole.root_node.children[i].metadata.size
        end
    end
    return nothing
end

function expand_abstraction!(expansion::SequenceChoiceVarExpansion, hole, holes, abstraction, config::SearchConfig)
    # take a hole (/seq <things> ...) and make it (/seq <things> ?? ...). Place the ?? above the ... on the stack,
    # but do *not* remove ... from the stack, since it will be consumed by the next expansion once the ?? is filled in

    insert_before_sequence_hole!(hole, holes) do i
        x = new_hole((hole, i))
        x.leaf = Symbol("?$(expansion.idx)")
        x
    end
    abstraction.choice_arity += 1
    # TODO make this more general
    @assert expansion.dfa_state == :S || expansion.dfa_state == :uninit_state
    push!(abstraction.dfa_choicevars, :seqS)

    abstraction.body_size += 1
end

function expand_match!(config::SearchConfig, expansion::SequenceChoiceVarExpansion, match::Match)::Vector{Match}
    not_consuming_hole = match

    @assert expansion.idx == length(match.choice_var_captures)

    # dont consume the hole
    push!(not_consuming_hole.choice_var_captures, SExpr[])


    # consume the hole
    # first check if there's more space in the sequence
    last_hole = match.holes[end]::RemainingSequenceHole
    results = Match[]
    captured_list = SExpr[]
    for count in 1:length(last_hole.root_node.children)-last_hole.num_consumed
        if count > config.max_choicevar_length
            break
        end
        consuming_hole = copy_match(match)

        pop!(consuming_hole.holes) === last_hole || error("no idea how this could happen")
        consuming_hole.holes_size -= last_hole.root_node.children[last_hole.num_consumed+count].metadata.size
        # push the hole back on the stack
        push!(consuming_hole.holes_stack, last_hole)

        new_sequence_hole = RemainingSequenceHole(last_hole.root_node, last_hole.num_consumed + count, last_hole.is_subseq)
        push!(consuming_hole.holes, new_sequence_hole)

        captured = new_sequence_hole.root_node.children[new_sequence_hole.num_consumed]::SExpr

        push!(captured_list, captured)

        consuming_hole.choice_var_captures[end] = copy(captured_list)

        push!(results, consuming_hole)
    end
    return results
end

function unexpand!(search_state::SearchState{Match}, expansion, hole)
    unexpand_abstraction!(expansion, hole, search_state.holes, search_state.abstraction, search_state.config)
    for match in search_state.matches
        unexpand_match!(expansion, match)
    end
end

function unexpand!(search_state::SearchState{MatchPossibilities}, expansion, hole)
    unexpand_abstraction!(expansion, hole, search_state.holes, search_state.abstraction, search_state.config)
    search_state.matches = pop!(search_state.matches_stack)
    for match_poss in search_state.matches
        for match in match_poss.alternatives
            unexpand_match!(expansion, match)
        end
    end
end

function unexpand_abstraction!(expansion::SyntacticLeafExpansion, hole, holes, abstraction, config::SearchConfig)
    hole.leaf = SYM_HOLE

    abstraction.body_size -= symbol_size(expansion.leaf, config.size_by_symbol)
end

function unexpand_match!(expansion::SyntacticLeafExpansion, match::Match)
    hole = pop!(match.holes_stack)::TreeNodeHole
    push!(match.holes, hole)

    match.holes_size += hole.metadata.size
end

function unexpand_abstraction!(expansion::SyntacticNodeExpansion, hole, holes, abstraction, config::SearchConfig)
    hole.leaf = SYM_HOLE

    # pop from .args and holes
    for i in 1:expansion.num_holes
        if expansion.head !== :no_expand_head && i == expansion.num_holes
            pop!(hole.children).leaf === expansion.head || error("expected same head")
        else
            pop!(hole.children).leaf === pop!(holes).leaf === SYM_HOLE || error("not a hole")
        end
    end

    if expansion.head !== :no_expand_head
        abstraction.body_size -= symbol_size(expansion.head, config.size_by_symbol)
    end
end

function unexpand_match!(expansion::SyntacticNodeExpansion, match::Match)
    num_remove = if expansion.head !== :no_expand_head
        expansion.num_holes - 1
    else
        expansion.num_holes
    end
    for _ in 1:num_remove
        pop!(match.holes)
    end

    hole = pop!(match.holes_stack)::TreeNodeHole
    length(hole.children) == expansion.num_holes || error("mismatched number of children to expand to; should be same though since expand!() checked this")
    push!(match.holes, hole)

    if expansion.head !== :no_expand_head
        match.holes_size += hole.children[1].metadata.size
    end
end

function unexpand_abstraction!(expansion::AbstractionExpansion, hole, holes, abstraction, config::SearchConfig)
    hole.leaf = SYM_HOLE
    if expansion.fresh
        abstraction.arity -= 1
        pop!(abstraction.dfa_metavars)
    end

    abstraction.body_size -= 1
end

function unexpand_match!(expansion::AbstractionExpansion, match::Match)
    hole = pop!(match.holes_stack)::TreeNodeHole
    push!(match.holes, hole)
    if expansion.fresh
        pop!(match.unique_args) === hole || error("expected same hole")
    end

    match.holes_size += hole.metadata.size
end

function unexpand_abstraction!(expansion::SymbolExpansion, hole, holes, abstraction, config::SearchConfig)
    # set the head symbol of the hole
    hole.leaf = SYM_HOLE
    if expansion.fresh
        abstraction.sym_arity -= 1
        pop!(abstraction.dfa_symvars)
    end

    abstraction.body_size -= 1
end

function unexpand_match!(expansion::SymbolExpansion, match::Match)
    hole = pop!(match.holes_stack)::TreeNodeHole
    push!(match.holes, hole)

    if expansion.fresh
        pop!(match.sym_of_idx)
        delete!(match.idx_of_sym, hole.leaf)
    end

    match.holes_size += hole.metadata.size
end

function unexpand_abstraction!(expansion::SequenceExpansion, hole, holes, abstraction, config::SearchConfig)
    pop!(holes) === hole || error("expected same hole")

    # remove the ... and /seq from the sequence
    is_seq_hole_token(pop!(hole.children)) || error("expected sequence hole token")
    head = pop!(hole.children).leaf
    if expansion.is_subseq
        head === SYM_SUBSEQ_HEAD || error("expected SYM_SUBSEQ_HEAD")
    else
        head === SYM_SEQ_HEAD || error("expected SYM_SEQ_HEAD")
    end

    # set the head symbol of the hole, to make it a ?? hole
    hole.leaf = SYM_HOLE

    abstraction.body_size -= symbol_size(head, config.size_by_symbol)
end

function unexpand_match!(expansion::SequenceExpansion, match::Match)
    # remove the ... hole
    sequence_hole = pop!(match.holes)::RemainingSequenceHole
    # get the original hole and put it back on the stack
    original_hole = pop!(match.holes_stack)::TreeNodeHole
    push!(match.holes, original_hole)
    # check that the ... hole is the same as the one we just popped
    @assert sequence_hole.num_consumed == 1
    @assert sequence_hole.root_node === original_hole

    if expansion.is_subseq && match.start_items !== nothing
        for i in 1:match.start_items
            match.holes_size += original_hole.children[i].metadata.size
        end
        match.start_items = nothing
    end
    # put back the /seq node
    match.holes_size += original_hole.children[1].metadata.size
end

function remove_inserted_before_sequence_hole!(check_fn, hole, holes)
    # undoes the effect of insert_before_sequence_hole!() by removing the hole that was inserted
    # before the ... hole. The check_fn is called on the hole that was removed, to make sure it is
    # the right one.

    # remove the ... hole from the list of holes
    is_seq_hole_token(pop!(hole.children)) || error("expected sequence hole token")

    # delete the <new> hole from the sequence
    check_fn(pop!(hole.children))

    # put the ... hole back onto the end
    new_sequence_hole = new_seq_hole((hole, length(hole.children)))
    push!(hole.children, new_sequence_hole)

    # delete the (/seq <extra> ...) hole
    pop!(holes) === hole || error("expected same sequence")
end

function unexpand_abstraction!(expansion::SequenceElementExpansion, hole, holes, abstraction, config::SearchConfig)
    # remove the ?? hole from the list of holes
    pop!(holes).leaf === SYM_HOLE || error("expected SYM_HOLE")

    remove_inserted_before_sequence_hole!(hole, holes) do m
        m.leaf == SYM_HOLE || error("expected SYM_HOLE")
    end

    # doesn't affect holes_size since we are just replacing a ?? with a ...
end

function unexpand_match!(expansion::SequenceElementExpansion, match::Match)
    # get rid of the ?? and ... holes
    pop!(match.holes) isa TreeNodeHole || error("expected TreeNodeHole")
    pop!(match.holes) isa RemainingSequenceHole || error("expected RemainingSequenceHole")
    # put the original ... hole back on the stack
    push!(match.holes, pop!(match.holes_stack))

    # doesn't affect holes_size since we are just replacing a ?? and ... with a ...
end

function unexpand_abstraction!(expansion::SequenceTerminatorExpansion, hole, holes, abstraction, config::SearchConfig)
    # just put a ... on the stack and at the end of the sequence
    new_hole = new_seq_hole((hole, length(hole.children) + 1))
    push!(hole.children, new_hole)

    # doesn't affect holes_size since we are just adding a ... that has no size
end

function unexpand_match!(expansion::SequenceTerminatorExpansion, match::Match)
    push!(match.holes, pop!(match.holes_stack))

    if expansion.is_subseq
        e = expr_of(match)
        for i in match.end_items+1:length(e.children)
            match.holes_size += e.children[i].metadata.size
        end

        match.end_items = nothing
    end
    # doesn't affect holes_size since we are just adding a ... that currently matches nothing
end

function unexpand_abstraction!(expansion::SequenceChoiceVarExpansion, hole, holes, abstraction, config::SearchConfig)
    remove_inserted_before_sequence_hole!(hole, holes) do m
        m.leaf == Symbol("?$(expansion.idx)") || error("expected Symbol(?$(expansion.idx)), got $m")
    end
    abstraction.choice_arity -= 1
    pop!(abstraction.dfa_choicevars)

    abstraction.body_size -= 1
end

function unexpand_match!(expansion::SequenceChoiceVarExpansion, match::Match)
    # we are operating on the non-consuming hole
    # just get rid of the element from the dictionary choice_var_captures
    pop!(match.choice_var_captures)

    @assert expansion.idx == length(match.choice_var_captures)
end

const MatchKey = Tuple{Vector{Int64},Vector{Symbol},Vector{Tuple{Symbol,Int64}}}

function collapse_equivalent_matches(expansion::Expansion, updated_matches)
    return nothing
end

function collapse_equivalent_matches(expansion::SequenceTerminatorExpansion, updated_matches)
    if length(updated_matches) <= 1
        return nothing
    end
    was_updated = false
    best_match_per_id = Dict{MatchKey,Match}()
    for m in updated_matches
        id = match_key(m)
        if haskey(best_match_per_id, id)
            was_updated = true
            if best_match_per_id[id].local_utility < m.local_utility
                best_match_per_id[id] = m
            end
        else
            best_match_per_id[id] = m
        end
    end
    if !was_updated
        return nothing
    end
    return [v for (_, v) in best_match_per_id]
end

function match_key(m::Match)::MatchKey
    # we consider matches equivalent if from now on they will be
    # identical in what expansions can be applied to them, and how
    # those expansions will affect the local utility.

    # the only state that matters for this is the holes as well as
    # arguments that can potentially be used for variable reuse
    # (i.e. the unique_args and symbolic arguments)
    unique_args = [x.metadata.struct_hash for x in m.unique_args]
    holes = [hole_struct_hash(x) for x in m.holes]
    return (unique_args, m.sym_of_idx, holes)
end

# ensure that the struct hash is unique for each hole
# since these are positive integers, we can dove-tail them
hole_struct_hash(x::TreeNodeHole) = (:TreeNodeHole, x.metadata.struct_hash)
hole_struct_hash(x::RemainingSequenceHole) = (:RemainingSequenceHole, (x.root_node.metadata.struct_hash << 32) + x.num_consumed)


# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function strictly_dominated(search_state)
    redundant_arg_elim(search_state) && return true
    arg_capture(search_state) && return true
    choice_var_always_used_or_not(search_state) && return true
    choice_vars_adjacent_to_each_other(search_state) && return true
    variables_at_front_of_root_sequence(search_state) && return true
    false
end

# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function redundant_arg_elim(search_state)
    search_state.config.no_opt_redundant_args && return false
    for i in 1:search_state.abstraction.arity
        for j in i+1:search_state.abstraction.arity
            if args_match(search_state.matches, i, j)
                return true
            end
        end
    end
    false
end

function args_match(matches::Vector{MatchPossibilities}, i, j)
    all(match_poss -> args_match(match_poss.alternatives, i, j), matches)
end

function args_match(matches::Vector{Match}, i, j)
    all(match -> match.unique_args[i].metadata.struct_hash == match.unique_args[j].metadata.struct_hash, matches)
end

# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function arg_capture(search_state)
    search_state.config.no_opt_arg_capture && return false
    for i in 1:search_state.abstraction.arity
        if arg_capture(search_state, match -> match.unique_args[i])
            return true
        end
    end
    false
end

function arg_capture(search_state::SearchState{Match}, arg_extract_fn)
    first_match = search_state.matches[1]
    first_match_expr = arg_extract_fn(first_match)
    if all(match -> same_in_context(first_match, match, first_match_expr, arg_extract_fn(match)), search_state.matches)
        return true
    end
    return false
end

function arg_capture(search_state::SearchState{MatchPossibilities}, arg_extract_fn)
    first_match = search_state.matches[1].alternatives[1]
    first_match_expr = arg_extract_fn(first_match)
    if all(
        match_poss -> all(match -> same_in_context(match, first_match, arg_extract_fn(match), first_match_expr), match_poss.alternatives),
        search_state.matches
    )
        return true
    end
    return false
end

function same_in_context(m1::Match, m2::Match, e1::Nothing, e2::SExpr)
    return false
end

function same_in_context(m1::Match, m2::Match, e1::SExpr, e2::SExpr)
    # returns true iff e1 and e2 are the same in the context of m1 and m2
    if e1.metadata.struct_hash == e2.metadata.struct_hash
        return true
    end
    if e1.metadata.struct_hash_no_symbol != e2.metadata.struct_hash_no_symbol
        return false
    end
    if e1.leaf !== nothing
        if e2.leaf === nothing
            return false
        end
        # @assert e1.leaf isa Symbol && string(e1.leaf)[1] == '&'
        if !(e1.leaf in m1.sym_of_idx) || !(e2.leaf in m2.sym_of_idx)
            return false
        end
        return m1.idx_of_sym[e1.leaf] == m2.idx_of_sym[e2.leaf]
    end
    if length(e1.children) != length(e2.children)
        return false
    end
    for (c1, c2) in zip(e1.children, e2.children)
        if !same_in_context(m1, m2, c1, c2)
            return false
        end
    end
    return true
end

function choice_var_always_used_or_not(search_state)
    # returns true iff there exists a choice variable that is always used or always not used
    # a choice variable that is always used can be replaced with a metavariable
    #       this holds since choice variables and metavariables draw from the same arity pool
    # a choice variable that is always not used can be removed
    search_state.config.no_opt_redundant_args && return false
    for i in 1:search_state.abstraction.choice_arity
        if choice_var_always_used_or_not(search_state, i)
            return true
        end
    end
    false
end

function choice_vars_adjacent_to_each_other(search_state::SearchState{T}) where {T}
    search_state.config.max_choicevar_length < typemax(Int) && return false
    search_state.abstraction.choice_arity < 2 && return false
    choice_vars_adjacent_to_each_other(search_state.abstraction.body)
end

function choice_vars_adjacent_to_each_other(expr::SExpr)
    expr.leaf === nothing || return false
    head = expr.children[1].leaf
    if head == SYM_SEQ_HEAD || head == SYM_SUBSEQ_HEAD
        return choice_vars_adjacent_to_each_other_at_top_level(expr.children[2:end])
    end
    return any(choice_vars_adjacent_to_each_other, expr.children)
end

function choice_vars_adjacent_to_each_other_at_top_level(exprs)
    for i in 1:length(exprs)-1
        if is_choice_var(exprs[i]) && is_choice_var(exprs[i+1])
            return true
        end
    end
    return false
end

function is_choice_var(expr)
    expr.leaf === nothing && return false
    expr.leaf == SYM_HOLE && return false
    l = string(expr.leaf)
    return l[1] == '?'
end

function choice_var_always_used_or_not(search_state::SearchState{Match}, i)
    first_match = search_state.matches[1]
    first_match_choice = first_match.choice_var_captures[i]
    if all(match -> compatible_captures(match, match.choice_var_captures[i], first_match, first_match_choice), search_state.matches)
        return true
    end
    return false
end

function choice_var_always_used_or_not(search_state::SearchState{MatchPossibilities}, i)
    first_match = search_state.matches[1].alternatives[1]
    first_match_choice = first_match.choice_var_captures[i]
    if all(match_poss -> all(match -> compatible_captures(match, match.choice_var_captures[i], first_match, first_match_choice), match_poss.alternatives), search_state.matches)
        return true
    end
    return false
end

function compatible_captures(m_a::Match, capture_a::Vector{SExpr}, m_b::Match, capture_b::Vector{SExpr})
    # returns true iff capture_a and capture_b are compatible, i.e., they can be replaced by a metavariable
    if length(capture_a) != length(capture_b)
        return false
    end
    if length(capture_a) <= 1
        # if there's 0 or 1 it can be replaced by a metavariable
        return true
    end
    # check if they are all the same up to renaming
    return all(i -> same_in_context(m_a, m_b, capture_a[i], capture_b[i]), 1:length(capture_a))
end

function variables_at_front_of_root_sequence(search_state)
    # returns true in one of the following child_states
    # 1. the root sequence is a /seq and the first element is a choice variable
    #       this is always worse than the case where you just use a subsequence
    # 2. the root sequence is a /subseq and the first element is a metavariable or a choice variable
    #       this is always worse than just having a shorter subsequence
    search_state.config.no_opt_rooted_sequence && return false
    ab = search_state.abstraction.body
    if ab.leaf !== nothing
        return false
    end
    first_child = ab.children[1]
    if !(first_child.leaf == SYM_SEQ_HEAD || first_child.leaf == SYM_SUBSEQ_HEAD)
        return false
    end
    if length(ab.children) < 2
        return false
    end
    second_child = ab.children[2]
    if second_child.leaf === nothing
        return false
    end
    return is_variable(second_child; no_metavar=first_child.leaf == SYM_SEQ_HEAD)
end

function is_variable(expr; no_metavar)
    if expr.leaf === nothing
        return false
    end
    if expr.leaf == SYM_HOLE
        return false
    end
    l = string(expr.leaf)
    if l[1] == '?'
        return true
    end
    return !no_metavar && l[1] == '#'
end

function is_single_task(search_state)
    first = expr_of(search_state.matches[1]).metadata.program.task
    all(match -> expr_of(match).metadata.program.task == first, search_state.matches)
end

mutable struct SamplingProcessor{F<:Function}
    keep_frac::Float32
    score::F
end

using StatsBase
function process_expansions!(search_state)
    weights = search_state.expansion_processor.score.(search_state.expansions, search_state)
    num_samples = max(Int(round(length(search_state.expansions) * processor.keep_frac)), min(2, length(search_state.expansions)))
    search_state.expansions = sample(search_state.expansions, Weights(weights), num_samples, replace=false)
end
