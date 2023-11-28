"""
An expression. See SExpr for the version that's always used - the definition is split into
SExprGeneric and SExpr because mutually recursive types are supported in julia so we can't
directly have Expr and Match that point to each other and without using generics.
"""
mutable struct SExprGeneric{D,M}
    leaf::Union{Symbol,Nothing}
    children::Vector{SExprGeneric{D,M}}
    parent::Union{Tuple{SExprGeneric{D,M},Int}, Nothing} # parent and which index of the child it is
    match::Union{D, Nothing}
    metadata::Union{M,Nothing}
end

mutable struct ProgramGeneric{D,M}
    expr::SExprGeneric{D,M}
    id::Int
    task::Int
end

mutable struct MetadataGeneric{D}
    program::ProgramGeneric{D,MetadataGeneric{D}} # which program this subtree appears in
    size::Float32
    num_nodes::Int
    struct_hash::Int
    dfa_state::Symbol
    # postorder location of the underlying node in the corpus.
    id::Int
end

mutable struct Match
    # represents a match of the current abstraction being constructed
    # match objects are created once at the start of each iteration, which each match
    #   corresponding to a node in the corpus
    #   not all match objects are active at any given time
    #   e.g., when ?? is the abstraction, every match object is active, but
    #   if it's (+ 2 ??), only the match objects for locations matching (+ 2 ??) are active
    # the match object contains information regarding match location specific information
    #   - what are the holes that need to be expanded at the current location, etc.
    # Fields:
    # pointer to subtree in original corpus
    expr::SExprGeneric{Match,MetadataGeneric{Match}}
    # pointers to first instance of each arg within subtree ie args[1] is the thing that #0 matches
    unique_args::Vector{SExprGeneric{Match,MetadataGeneric{Match}}}
    # pointer to the place that each hole matches.
    holes::Vector{SExprGeneric{Match,MetadataGeneric{Match}}}
    # history of the holes
    holes_stack::Vector{SExprGeneric{Match,MetadataGeneric{Match}}}
    # history of the local utilities of the match
    local_utility_stack::Vector{Float32}


    # Local utility: utility if you rewrite at this location specifically. Match specific
    # Tracks Eqn 12: https://arxiv.org/pdf/2211.16605.pdf
    local_utility::Float32
    
    # handling rewrite conflicts. When the same abstraction can be used in two overlapping places, we need to pick one.
    # e.g., program = (foo (foo (foo x))). abstraction = (foo (foo #0)). abstraction can either match
    # as (fn_1 (foo x)) or (foo (fn_1 x)). We need to pick one.

    # Simple bottom-up dynamic programming to figure out which is best
    # TODO move to a separate rewritematch struct?
    cumulative_utility::Float32
    accept_rewrite::Bool
    is_active::Bool

    # conversions between a symbol &foo and it's index %0
    sym_of_idx::Vector{Symbol}
    idx_of_sym::Dict{Symbol, Int} # idx_of_sym[sym_of_idx[i]] == i
    idx_is_fresh::Vector{Bool} # stack of whether each idx is fresh across the levels of search, used for backtracking

    # metavariable for continuation
    continuation::Union{Nothing,SExprGeneric{Match,MetadataGeneric{Match}}}

    Match(expr, id, config) = new(
        expr,
        SExprGeneric{Match,MetadataGeneric{Match}}[],
        [expr],
        SExprGeneric{Match,MetadataGeneric{Match}}[],
        Float32[],
        local_utility_init(config),
        NaN32,
        false,
        false,
        Symbol[],
        Dict{Symbol,Int}(),
        Bool[],
        nothing
    )
end

const Metadata = MetadataGeneric{Match}
const SExpr = SExprGeneric{Match,Metadata}
const Program = ProgramGeneric{Match,Metadata}

function sexpr_node(children::Vector{SExpr}; parent=nothing)
    expr = SExpr(nothing, children, parent, nothing, nothing)
    for (i,child) in enumerate(children)
        isnothing(child.parent) || error("arg already has parent")
        child.parent = (expr,i)
    end
    expr 
end

function sexpr_leaf(leaf::Symbol; parent=nothing)
    SExpr(leaf, Vector{SExpr}(), parent, nothing, nothing)
end

is_leaf(e::SExpr) = !isnothing(e.leaf)

# TODO document why isn't this copying all the fields??
function Base.copy(e::SExpr)
    SExpr(
        e.leaf,
        [copy(child) for child in e.children],
        nothing,
        nothing,
        nothing,
    )
end

@auto_hash_equals struct HashNode
    leaf::Union{Symbol,Nothing}
    children::Vector{Int}
end

const global_struct_hash = Dict{HashNode, Int}()

"""
sets structural hash value, possibly with side effects of updating the structural hash, and
sets e.metadata.struct_hash. Requires .metadata to be set so we know this will be used immutably
"""
function struct_hash(e::SExpr) :: Int
    isnothing(e.metadata) || isnothing(e.metadata.struct_hash) || return e.metadata.struct_hash

    node = HashNode(e.leaf, map(struct_hash,e.children))
    if !haskey(global_struct_hash, node)
        global_struct_hash[node] = length(global_struct_hash) + 1
    end
    isnothing(e.metadata) || (e.metadata.struct_hash = global_struct_hash[node])
    return global_struct_hash[node]
end


"""
Checks if one expression could be expanded to obtain another expression
"""
function could_expand_to(ancestor::SExpr, descendant::SExpr)
    is_hole(ancestor) && return true
    is_leaf(ancestor) && return ancestor.leaf === descendant.leaf
    length(ancestor.children) == length(descendant.children) || return false
    for (a,d) in zip(ancestor.children, descendant.children)
        could_expand_to(a,d) || return false
    end
    true
end



const SYM_HOLE = Symbol("??")
new_hole(parent_and_argidx) = sexpr_leaf(SYM_HOLE; parent=parent_and_argidx)


is_hole(e::SExpr) = e.leaf === SYM_HOLE

"child-first traversal"
function subexpressions(e::SExpr; subexprs = SExpr[])
    for child in e.children
        subexpressions(child, subexprs=subexprs)
    end
    push!(subexprs, e)
end

size(e::SExpr, size_by_symbol) = size(e.leaf, size_by_symbol) + sum(x -> size(x, size_by_symbol), e.children, init=0.0)
function size(leaf::Symbol, size_by_symbol)::Float32
    symbol_size(leaf, size_by_symbol)
end
size(leaf::Nothing, size_by_symbol) = 0.0

num_nodes(e::SExpr) = 1 + sum(num_nodes, e.children, init=0)

# function size_no_abstraction_var(e::SExpr) :: Float32
#     if startswith(string(e.head), "#")
#         return 0
#     end
#     if isempty(e.args)
#         return 1.
#     end
#     return size(e.head) + sum(size_no_abstraction_var, e.args)
# end


Base.show(io::IO, e::SExpr) = begin    
    if is_leaf(e)
        print(io, e.leaf)
        @assert isempty(e.children)
    # elseif e.leaf === :app
        # print(io, "(", join(uncurry(e), " "), ")")
    else
        print(io, "(")
        for i in eachindex(e.children)
            print(io, e.children[i])
            i != lastindex(e.children) && print(io, " ")
        end
        print(io, ")")
        # print(io, "(", join(e.children, " "), ")")
    end
end

"""
takes (app (app f x) y) and returns (f x y)
"""
function uncurry_app(e::SExpr)
    (length(e.children) != 3 || e.children[1].leaf !== :app) && return[e]
    res = uncurry_app(e.children[2])
    return push!(res, e.children[3])
end

"""
takes ((f x) y) and returns (f x y)

of course (f (g x)) stays as is

(f (g x) y)

"""
function uncurry(e::SExpr)
    is_leaf(e) && return copy(e)
    @assert length(e.children) == 2
    f = uncurry(e.children[1])
    x = uncurry(e.children[2])
    if is_leaf(f)
        return sexpr_node([f, x])
    else
        return push_child!(f, x)
    end
end

"""
Binarizes an expression. Starting from (f x y) the result is ((f x) y).
Applications are implicit in the tree structure, there are not explicit :app symbols
as otherwise these might get abstracted over and aren't particularly useful
"""
function curry(e::SExpr) :: SExpr
    is_leaf(e) && return copy(e)
    @assert length(e.children) > 1

    expr = curry(e.children[1])
    for child in e.children[2:end]
        expr = sexpr_node([expr, curry(child)])
    end
    expr
end

function push_child!(parent::SExpr, child::SExpr)
    push!(parent.children, child)
    child.parent = (parent, length(parent.children))
    parent
end

"""
Parse a string into an SExpr. Uses lisp-like syntax.

"foo" -> SAtom(:foo)
"(foo)" -> SList([SAtom(:foo)])
"((foo))" -> SList([SList([SAtom(:foo)])]) 
"(foo bar baz)" -> SList([SAtom(:foo), SAtom(:bar), SAtom(:baz)])
"()" -> SList([])

"""
function Base.parse(::Type{SExpr}, original_s::String)
    # add guaranteed parens around whole thing and guaranteed spacing around parens so they parse into their own items
    s = replace(original_s, "(" => " ( ", ")" => " ) ")

    # `split` will skip all quantities of all forms of whitespace
    items = split(s)
    length(items) > 2 || error("SExpr parse called on empty (or all whitespace) string")
    items[1] != ")" || error("SExpr starts with a closeparen. Found in $original_s")

    # this is a single symbol like "foo" or "bar"
    length(items) == 1 && return sexpr_leaf(Symbol(items[1]))

    i=0
    expr_stack = SExpr[]
    # num_open_parens = Int[]

    while true
        i += 1

        i <= length(items) || error("unbalanced parens: unclosed parens in $original_s")

        if items[i] == "("
            push!(expr_stack, sexpr_node(SExpr[]))
        elseif items[i] == ")"
            # end an expression: pop the last SExpr off of expr_stack and add it to the SExpr at one level before that

            if length(expr_stack) == 1
                i == length(items) || error("trailing characters after final closeparen in $original_s")
                break
            end

            last = pop!(expr_stack)
            push!(expr_stack[end].children, last)
        else
            # any other item like "foo" or "+" is a symbol
            push!(expr_stack[end].children, sexpr_leaf(Symbol(items[i])))
        end
    end

    length(expr_stack) != 0 || error("unreachable - should have been caught by the first check for string emptiness")
    length(expr_stack) == 1 || error("unbalanced parens: not enough close parens in $original_s")

    return pop!(expr_stack)
end