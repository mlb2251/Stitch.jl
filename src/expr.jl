"""
An expression. See SExpr for the version that's always used - the definition is split into
SExprGeneric and SExpr because mutually recursive types are supported in julia so we can't
directly have Expr and Match that point to each other and without using generics.
"""
mutable struct SExprGeneric{D}
    leaf::Union{Symbol,Nothing}
    children::Vector{SExpr{D}}
    parent::Union{(SExpr,Int), Nothing} # parent and which index of the child it is
    match::Union{D, Nothing}
end

mutable struct Match
    expr::SExprGeneric{Match} # pointer to subtree in original corpus
    all_args::Vector{SExprGeneric{Match}}
    unique_args::Vector{SExprGeneric{Match}} # pointers to first instance of each arg within subtree ie args[1] is #0
    holes::Vector{SExprGeneric{Match}} # pointers to holes within subtree
    holes_stack::Vector{SExprGeneric{Match}} # expanded holes
    local_utility_stack::Vector{Float32} # past utilities

    program::Program # which program this subtree appears in
    size::Float32
    num_nodes::Int
    struct_hash::Int

    # Tracks Eqn 12: https://arxiv.org/pdf/2211.16605.pdf
    local_utility::Float32
    
    cumulative_utility::Float32
    accept_rewrite::Bool
    is_active::Bool
    id::Int

    # conversions between a symbol &foo and it's index %0
    sym_of_idx::Vector{Symbol}
    idx_of_sym::Dict{Symbol, Int} # idx_of_sym[sym_of_idx[i]] == i
    idx_is_fresh::Vector{Bool} # stack of whether each idx is fresh across the levels of search, used for backtracking

    Match(expr, program, id) = new(expr, [], [], [expr], [], [], program, size(expr), num_nodes(expr), struct_hash(expr), local_utility_init(), NaN32, false, false, id, Symbol[], Dict{Symbol, Int}(), Bool[])
end

const SExpr = SExprGeneric{Match}

function sexpr_node(children::Vector{SExpr}; parent=nothing)
    expr = SExpr(nothing, children, parent, nothing)
    for (i,arg) in enumerate(args)
        isnothing(arg.parent) || error("arg already has parent")
        arg.parent = (expr,i)
    end
    expr 
end

function sexpr_leaf(leaf::Symbol; parent=nothing)
    SExpr(leaf, Vector{SExpr}(), parent, nothing)
end

is_leaf(e::SExpr) = !isnothing(e.leaf)

function Base.copy(e::SExpr)
    SExpr(
        e.leaf,
        [copy(child) for child in e.children]
        nothing,
        nothing
    )
end

@auto_hash_equals struct HashNode
    head::Symbol
    args::Vector{Int}
end

const global_struct_hash = Dict{HashNode, Int}()

"""
sets structural hash value, possibly with side effects of updating the structural hash, and
sets e.match.struct_hash. Requires .match to be set so we know this will be used immutably
"""
function struct_hash(e::SExpr) :: Int
    isnothing(e.match) || isnothing(e.match.struct_hash) || return e.match.struct_hash

    node = HashNode(e.head, map(struct_hash,e.args))
    if !haskey(global_struct_hash, node)
        global_struct_hash[node] = length(global_struct_hash) + 1
    end
    isnothing(e.match) || (e.match.struct_hash = global_struct_hash[node])
    return global_struct_hash[node]
end

function curried_application(f::Symbol, args) :: SExpr
    expr = SExpr(f)
    for arg in args
        expr = SExpr(:app, args=[expr, arg])
    end
    expr
end


new_hole(parent, arg_idx) = sexpr_leaf(Symbol("??"); parent=(parent, arg_idx))

"child-first traversal"
function subexpressions(e::SExpr; subexprs = SExpr[])
    for child in e.children
        subexpressions(child, subexprs=subexprs)
    end
    push!(subexprs, e)
end

size(e::SExpr) = size(e.leaf) + sum(size, e.children, init=0.)
size(leaf::Symbol) = 1.
size(leaf::Nothing) = 0.

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
    if isempty(e.args)
        print(io, e.head)
    elseif e.head === :app
        print(io, "(", join(uncurry(e), " "), ")")
    else
        print(io, "(", e.head, " ", join(e.args, " "), ")")
    end
end


"""
takes (app (app f x) y) and returns [f, x, y]
"""
function uncurry(e::SExpr)
    if e.head === :app
        res = uncurry(e.args[1])
        return push!(res, e.args[2])
    else
        return [e]
    end
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
    length(items) == 1 && return SExpr(Symbol(items[1]))

    i=0
    expr_stack = SExpr[]
    # num_open_parens = Int[]

    while true
        i += 1

        i <= length(items) || error("unbalanced parens: unclosed parens in $original_s")

        if items[i] == "("
            # begin a new expression: push a new SExpr + head onto expr_stack
            i += 1
            i <= length(items) || error("unbalanced parens: too many open parens in $original_s")
            items[i] != ")" || error("Empty parens () are not allowed. Found in $original_s")
            items[i] != "(" || error("Each open paren must be followed directly by a head symbol like (foo ...) or ( foo ...) but instead another open paren instead like ((...) ...). Error found in $original_s")
            push!(expr_stack, SExpr(Symbol(items[i])))
        elseif items[i] == ")"
            # end an expression: pop the last SExpr off of expr_stack and add it to the SExpr at one level before that

            if length(expr_stack) == 1
                i == length(items) || error("trailing characters after final closeparen in $original_s")
                break
            end

            length(expr_stack) >= 2 || error("unbalanced parens: too many close parens in $original_s")

            last = pop!(expr_stack)
            push!(expr_stack[end].args, last)
        else
            # any other item like "foo" or "+" is a symbol
            push!(expr_stack[end].args, SExpr(Symbol(items[i])))
        end
    end

    length(expr_stack) != 0 || error("unreachable - should have been caught by the first check for string emptiness")
    length(expr_stack) == 1 || error("unbalanced parens: not enough close parens in $original_s")

    return pop!(expr_stack)
end