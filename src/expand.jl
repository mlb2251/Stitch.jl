using Random

function possible_expansions!(search_state)
    isempty(search_state.expansions) || error("expansions should be empty")

    expansions!(SyntacticLeafExpansion, search_state)
    expansions!(AbstractionExpansion, search_state)
    expansions!(SymbolExpansion, search_state)
    expansions!(ContinuationExpansion, search_state)
    if search_state.config.match_sequences
        expansions!(SequenceExpansion, search_state)
        expansions!(SequenceElementExpansion, search_state)
        expansions!(SequenceTerminatorExpansion, search_state)
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

function expansions!(typ, search_state)
    flattened_matches = [(i, m) for (i, m) in enumerate(search_state.matches)]
    res = collect_expansions(typ, search_state.abstraction, flattened_matches, search_state.config)
    for (expansion, tagged_matches) in res
        push!(search_state.expansions, PossibleExpansion(
            [m for (_, m) in tagged_matches],
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
        if typeof(match.holes[end]) != TreeNodeHole
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
    matches_of_idx = Dict{Int,Vector{Tuple{Int,Match}}}()
    freshness_of_idx = Dict{Int,Bool}()
    sym_of_idx = Dict{Int,Symbol}()
    for (i, match) in matches
        if typeof(match.holes[end]) != TreeNodeHole
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
            []
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

    matches = filter(((_, m),) -> typeof(m.holes[end]) == TreeNodeHole, matches)

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

        if abstraction.arity < config.max_arity
            # fresh variable
            push!(result, (AbstractionExpansion(abstraction.arity, true, sym), ms))
        end

    end

    if isnothing(config.dfa)
        collect_abstraction_expansions_for_dfa_state!(matches, :uninit_state)
    else
        matches_e = Vector{Tuple{Int,Match}}()
        matches_s = Vector{Tuple{Int,Match}}()
        for (i, match) in matches
            hole = match.holes[end]
            if typeof(hole) != TreeNodeHole
                continue
            end
            dfa_state = hole.metadata.dfa_state
            if dfa_state === :E
                push!(matches_e, (i, match))
            elseif dfa_state === :S
                push!(matches_s, (i, match))
            end
        end
        collect_abstraction_expansions_for_dfa_state!(matches_e, :E)
        collect_abstraction_expansions_for_dfa_state!(matches_s, :S)
    end
    result
end

function collect_expansions(
    ::Type{ContinuationExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    if length(matches) == 0
        return []
    end

    node = matches[1][2].holes[end]
    if typeof(node) != TreeNodeHole
        return []
    end
    isnothing(node.parent) && return [] # no identity abstraction allowed

    while !isnothing(node.parent)
        (parent, i) = node.parent
        # check that our parent is a semicolon and we are the righthand child
        # so in particular parent == (semi _ node)
        i == 3 || return []
        parent.children[1] === :semi || return []
        node = parent
    end

    isnothing(matches[1][2].continuation) || error("shouldn't be possible to have two continuation expansions!")

    return [(ContinuationExpansion(), matches)]
end

function collect_expansions(
    ::Type{SequenceExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    matches = filter(matches) do (i, match)
        if typeof(match.holes[end]) != TreeNodeHole
            return false
        end
        !is_leaf(match.holes[end]) && match.holes[end].children[1].leaf === SYM_SEQ_HEAD
    end
    if length(matches) == 0
        return []
    end
    return [(SequenceExpansion(), matches)]
end

function collect_expansions(
    ::Type{SequenceElementExpansion},
    abstraction::Abstraction,
    matches::Vector{Tuple{Int,Match}},
    config
)::Vector{Tuple{Expansion,Vector{Tuple{Int,Match}}}}

    matches = filter(matches) do (_, match)
        hole = match.holes[end]
        if typeof(hole) != RemainingSequenceHole
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

    matches = filter(matches) do (_, match)
        hole = match.holes[end]
        if typeof(hole) != RemainingSequenceHole
            return false
        end
        hole.num_consumed == length(hole.root_node.children)
    end
    if length(matches) == 0
        return []
    end
    return [(SequenceTerminatorExpansion(), matches)]
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
    expand!(search_state, expansion, hole)

    for match in expansion.matches
        # save the local utility for backtracking
        push!(match.local_utility_stack, match.local_utility)
        match.local_utility += delta_local_utility(search_state.config, match, expansion)
    end

    check_number_of_holes(search_state)
end

function unexpand_general!(search_state::SearchState)

    hole = pop!(search_state.holes_stack)
    # hole_dfa_state = pop!(search_state.hole_dfa_states_stack);

    # pop the expansion to undo
    expansion = pop!(search_state.past_expansions)

    # unexpand - this should be an inverse to expand!()
    search_state.matches === expansion.matches || error("mismatched matches")
    for match in search_state.matches
        match.local_utility = pop!(match.local_utility_stack)
    end

    unexpand!(search_state, expansion, hole)
    # restore other state
    search_state.expansions = pop!(search_state.expansions_stack)
    search_state.matches = pop!(search_state.matches_stack)
    push!(search_state.holes, hole)
    # push!(search_state.hole_dfa_states, hole_dfa_state)

    check_number_of_holes(search_state)
    # all(match -> length(match.args) == length(search_state.args), search_state.matches) || error("mismatched number of holes")    

end

function check_number_of_holes(search_state)
    all(match -> length(match.holes) == length(search_state.holes), search_state.matches) || error("mismatched number of holes")
end

function expand!(search_state, expansion, hole)

    expand_abstraction!(expansion, hole, search_state.holes, search_state.abstraction)
    for match in search_state.matches
        expand_match!(expansion, match)
    end
end

function expand_abstraction!(expansion::PossibleExpansion{SyntacticLeafExpansion}, hole, holes, abstraction)
    # set the head symbol of the hole
    hole.leaf = expansion.data.leaf
end

function expand_match!(expansion::PossibleExpansion{SyntacticLeafExpansion}, match)
    hole = pop!(match.holes)
    push!(match.holes_stack, hole)
    @assert is_leaf(hole)
end
function expand_abstraction!(expansion::PossibleExpansion{SyntacticNodeExpansion}, hole, holes, abstraction)
    # make it no longer a leaf
    hole.leaf = nothing

    # add fresh holes to .args and also keep track of them in search_state.abstraction.holes
    for i in 1:expansion.data.num_holes
        h = new_hole((hole, i))
        push!(hole.children, h)
        if i == 1 && expansion.data.head !== :no_expand_head
            # set the head symbol of the hole and dont push it to the list of search state holes
            h.leaf = expansion.data.head
        else
            push!(holes, h)
            # state = if expansion.data.head === :no_expand_head || isnothing(search_state.config.dfa)
            #     :no_dfa_state
            # else
            #     @show i
            #     @show dfa_state
            #     @show expansion.data.head
            #     @show search_state.config.dfa[dfa_state][expansion.data.head]
            #     search_state.config.dfa[dfa_state][expansion.data.head][i-1]
            # end
            # push!(search_state.hole_dfa_states, state)
        end
    end

    # reverse holes so they go left to right
    # @views reverse!(search_state.holes[end-expansion.data.num_holes+1:end]) 
end

function expand_match!(expansion::PossibleExpansion{SyntacticNodeExpansion}, match)
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)
    length(hole.children) == expansion.data.num_holes || error("mismatched number of children to expand to at location: $(match.expr) with hole $hole for expansion $(expansion.data)")
    push!(match.holes_stack, hole)

    # add all the children of the hole as new holes (except possibly the head)
    if expansion.data.head !== :no_expand_head
        append!(match.holes, hole.children[2:end])
    else
        append!(match.holes, hole.children)
    end
end


function expand_abstraction!(expansion::PossibleExpansion{AbstractionExpansion}, hole, holes, abstraction)
    hole.leaf = Symbol("#$(expansion.data.index)")

    if expansion.data.fresh
        abstraction.arity += 1
        push!(abstraction.dfa_metavars, expansion.data.dfa_state)
    end
end

function expand_match!(expansion::PossibleExpansion{AbstractionExpansion}, match)
    hole = pop!(match.holes)
    push!(match.holes_stack, hole)
    if expansion.data.fresh
        push!(match.unique_args, hole) # move the hole to be an argument
    end
end

function expand_abstraction!(expansion::PossibleExpansion{SymbolExpansion}, hole, holes, abstraction)
    # set the head symbol of the hole
    hole.leaf = Symbol("%$(expansion.data.idx)")

    if expansion.data.fresh
        abstraction.sym_arity += 1
        push!(abstraction.dfa_symvars, expansion.data.dfa_state)
    end
end

function expand_match!(expansion::PossibleExpansion{SymbolExpansion}, match)
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)
    push!(match.holes_stack, hole)

    @assert string(hole.leaf)[1] == '&'

    if expansion.data.fresh
        # this is a new symbol
        push!(match.sym_of_idx, hole.leaf)
        match.idx_of_sym[hole.leaf] = expansion.data.idx
    end
end


function expand_abstraction!(expansion::PossibleExpansion{ContinuationExpansion}, hole, holes, abstraction)
    # set the head symbol of the hole
    hole.leaf = Symbol("#continuation")
end

function expand_match!(expansion::PossibleExpansion{ContinuationExpansion}, match)
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)
    push!(match.holes_stack, hole)
    @assert isnothing(match.continuation)
    match.continuation = hole
end

function expand_abstraction!(expansion::PossibleExpansion{SequenceExpansion}, hole, holes, abstraction)
    # take a hole ?? and make it (/seq ...). The hole is then pushed to the stack
    hole.leaf = nothing
    head = new_hole((hole, 1))
    head.leaf = SYM_SEQ_HEAD
    nh = new_seq_hole((hole, 2))

    push!(hole.children, head)
    push!(hole.children, nh)

    push!(holes, hole)
end

function expand_match!(expansion::PossibleExpansion{SequenceExpansion}, match)
    # pop next hole and save it for future backtracking
    hole = pop!(match.holes)
    push!(match.holes_stack, hole)
    # add a hole representing the remaining sequence
    push!(match.holes, RemainingSequenceHole(hole, 1))
end

function expand_abstraction!(expansion::PossibleExpansion{SequenceElementExpansion}, hole, holes, abstraction)
    # take a hole (/seq <things> ...) and make it (/seq <things> ?? ...). Place the ?? above the ... on the stack,
    # but do *not* remove ... from the stack, since it will be consumed by the next expansion once the ?? is filled in

    i = length(hole.children)
    element_hole = new_hole((hole, i))
    # overwrite the old ...
    hole.children[i] = element_hole

    new_sequence_hole = new_seq_hole((hole, i + 1))
    push!(hole.children, new_sequence_hole)

    push!(holes, hole)
    push!(holes, element_hole)
end

function expand_match!(expansion::PossibleExpansion{SequenceElementExpansion}, match)
    last_hole = pop!(match.holes)
    @assert typeof(last_hole) == RemainingSequenceHole
    # push the hole back on the stack
    push!(match.holes_stack, last_hole)

    new_sequence_hole = RemainingSequenceHole(last_hole.root_node, last_hole.num_consumed + 1)
    push!(match.holes, new_sequence_hole)
    push!(match.holes, new_sequence_hole.root_node.children[new_sequence_hole.num_consumed])
end

function expand_abstraction!(expansion::PossibleExpansion{SequenceTerminatorExpansion}, hole, holes, abstraction)
    # just remove the last hole, and implicitly close off the sequence
    pop!(hole.children)
end

function expand_match!(expansion::PossibleExpansion{SequenceTerminatorExpansion}, match)
    # pop next hole and save it for future backtracking
    last_hole = pop!(match.holes)
    @assert typeof(last_hole) == RemainingSequenceHole
    @assert last_hole.num_consumed == length(last_hole.root_node.children)
    push!(match.holes_stack, last_hole)
end

function unexpand!(search_state, expansion, hole)
    unexpand_abstraction!(expansion, hole, search_state.holes, search_state.abstraction)
    for match in search_state.matches
        unexpand_match!(expansion, match)
    end
end

function unexpand_abstraction!(expansion::PossibleExpansion{SyntacticLeafExpansion}, hole, holes, abstraction)
    hole.leaf = SYM_HOLE
end

function unexpand_match!(expansion::PossibleExpansion{SyntacticLeafExpansion}, match)
    hole = pop!(match.holes_stack)
    push!(match.holes, hole)
end

function unexpand_abstraction!(expansion::PossibleExpansion{SyntacticNodeExpansion}, hole, holes, abstraction)
    hole.leaf = SYM_HOLE

    # pop from .args and holes
    for i in 1:expansion.data.num_holes
        if expansion.data.head !== :no_expand_head && i == expansion.data.num_holes
            pop!(hole.children).leaf === expansion.data.head || error("expected same head")
        else
            pop!(hole.children).leaf === pop!(holes).leaf === SYM_HOLE || error("not a hole")
        end
    end
end

function unexpand_match!(expansion::PossibleExpansion{SyntacticNodeExpansion}, match)
    num_remove = if expansion.data.head !== :no_expand_head
        expansion.data.num_holes - 1
    else
        expansion.data.num_holes
    end
    for _ in 1:num_remove
        pop!(match.holes)
    end

    hole = pop!(match.holes_stack)
    length(hole.children) == expansion.data.num_holes || error("mismatched number of children to expand to; should be same though since expand!() checked this")
    push!(match.holes, hole)
end

function unexpand_abstraction!(expansion::PossibleExpansion{AbstractionExpansion}, hole, holes, abstraction)
    hole.leaf = SYM_HOLE
    if expansion.data.fresh
        abstraction.arity -= 1
        pop!(abstraction.dfa_metavars)
    end
end

function unexpand_match!(expansion::PossibleExpansion{AbstractionExpansion}, match)
    hole = pop!(match.holes_stack)
    push!(match.holes, hole)
    if expansion.data.fresh
        pop!(match.unique_args) === hole || error("expected same hole")
    end
end

function unexpand_abstraction!(expansion::PossibleExpansion{SymbolExpansion}, hole, holes, abstraction)
    # set the head symbol of the hole
    hole.leaf = SYM_HOLE
    if expansion.data.fresh
        abstraction.sym_arity -= 1
        pop!(abstraction.dfa_symvars)
    end
end

function unexpand_match!(expansion::PossibleExpansion{SymbolExpansion}, match)
    hole = pop!(match.holes_stack)
    push!(match.holes, hole)

    if expansion.data.fresh
        pop!(match.sym_of_idx)
        delete!(match.idx_of_sym, hole.leaf)
    end
end

function unexpand_abstraction!(expansion::PossibleExpansion{ContinuationExpansion}, hole, holes, abstraction)
    hole.leaf = SYM_HOLE
end

function unexpand_match!(expansion::PossibleExpansion{ContinuationExpansion}, match)
    hole = pop!(match.holes_stack)
    push!(match.holes, hole)
    @assert match.continuation === hole
    match.continuation = nothing
end

function unexpand_abstraction!(expansion::PossibleExpansion{SequenceExpansion}, hole, holes, abstraction)
    pop!(holes) === hole || error("expected same hole")

    # remove the ... and /seq from the sequence
    is_seq_hole_token(pop!(hole.children)) || error("expected sequence hole token")
    pop!(hole.children).leaf === SYM_SEQ_HEAD || error("expected SYM_SEQ_HEAD")

    # set the head symbol of the hole, to make it a ?? hole
    hole.leaf = SYM_HOLE

end

function unexpand_match!(expansion::PossibleExpansion{SequenceExpansion}, match)
    # remove the ... hole
    sequence_hole = pop!(match.holes)
    # get the original hole and put it back on the stack
    original_hole = pop!(match.holes_stack)
    push!(match.holes, original_hole)
    # check that the ... hole is the same as the one we just popped
    @assert typeof(sequence_hole) == RemainingSequenceHole
    @assert sequence_hole.num_consumed == 1
    @assert sequence_hole.root_node === original_hole
end

function unexpand_abstraction!(expansion::PossibleExpansion{SequenceElementExpansion}, hole, holes, abstraction)
    # remove the ?? hole from the list of holes
    pop!(holes).leaf == SYM_HOLE || error("expected SYM_HOLE")

    # remove the ... hole from the list of holes
    is_seq_hole_token(pop!(hole.children)) || error("expected sequence hole token")

    # delete the ?? hole from the sequence
    pop!(hole.children).leaf == SYM_HOLE || error("expected SYM_HOLE")

    # put the ... hole back onto the end
    new_sequence_hole = new_seq_hole((hole, length(hole.children)))
    push!(hole.children, new_sequence_hole)

    # delete the (/seq <extra> ...) hole
    pop!(holes) === hole || error("expected same sequence")
end

function unexpand_match!(expansion::PossibleExpansion{SequenceElementExpansion}, match)
    # get rid of the ?? and ... holes
    typeof(pop!(match.holes)) == TreeNodeHole || error("expected TreeNodeHole")
    typeof(pop!(match.holes)) == RemainingSequenceHole || error("expected RemainingSequenceHole")
    # put the original ... hole back on the stack
    push!(match.holes, pop!(match.holes_stack))
end

function unexpand_abstraction!(expansion::PossibleExpansion{SequenceTerminatorExpansion}, hole, holes, abstraction)
    # just put a ... on the stack and at the end of the sequence
    new_hole = new_seq_hole((hole, length(hole.children) + 1))
    push!(hole.children, new_hole)
end

function unexpand_match!(expansion::PossibleExpansion{SequenceTerminatorExpansion}, match)
    push!(match.holes, pop!(match.holes_stack))
end

# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function strictly_dominated(search_state)
    redundant_arg_elim(search_state) || arg_capture(search_state)
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

function args_match(matches::Vector{Match}, i, j)
    all(match -> match.unique_args[i].metadata.struct_hash == match.unique_args[j].metadata.struct_hash, matches)
end

# https://arxiv.org/pdf/2211.16605.pdf (section 4.3)
function arg_capture(search_state)
    search_state.config.no_opt_arg_capture && return false
    for i in 1:search_state.abstraction.arity
        first_match = search_state.matches[1].unique_args[i].metadata.struct_hash
        if all(match -> match.unique_args[i].metadata.struct_hash == first_match, search_state.matches)
            return true
        end
    end
    false
end

function is_single_task(search_state)
    first = search_state.matches[1].expr.metadata.program.task
    all(match -> match.expr.metadata.program.task == first, search_state.matches)
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
