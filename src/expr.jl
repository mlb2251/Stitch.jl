"""
An expression. Contains parent pointer
"""
mutable struct SExpr{D}
    head::Symbol
    args::Vector{SExpr{D}}

    parent::Union{SExpr, Nothing}
    arg_idx::Union{Int, Nothing}

    data::Union{D, Nothing}

    # type annotation on args forces the more efficient passing in of a SExpr vector
    function SExpr(head; args = Vector{SExpr{Match}}(), parent=nothing)
        expr = new{Match}(head, args, parent, nothing, nothing)
        for (i,arg) in enumerate(args)
            isnothing(arg.parent) || error("arg already has parent")
            arg.parent = expr
            arg.arg_idx = i
        end
        expr
    end
end

function Base.copy(e::SExpr)
    SExpr(e.head, args=[copy(arg) for arg in e.args])
end

# getproperty!(e::SExpr, f::Symbol) = getfield(e, f)

@auto_hash_equals struct HashNode
    head::Symbol
    args::Vector{Int}
end

const global_struct_hash = Dict{HashNode, Int}()

"""
sets structural hash value, possibly with side effects of updating the structural hash, and
sets e.data.struct_hash. Requires .data to be set so we know this will be used immutably
"""
function struct_hash(e::SExpr) :: Int
    isnothing(e.data) || isnothing(e.data.struct_hash) || return e.data.struct_hash

    node = HashNode(e.head, map(struct_hash,e.args))
    if !haskey(global_struct_hash, node)
        global_struct_hash[node] = length(global_struct_hash) + 1
    end
    isnothing(e.data) || (e.data.struct_hash = global_struct_hash[node])
    return global_struct_hash[node]
end

function curried_application(f::Symbol, args) :: SExpr
    expr = SExpr(f)
    for arg in args
        expr = SExpr(:app, args=[expr, arg])
    end
    expr
end


new_hole(parent) = SExpr(Symbol("??"), parent=parent)

"child-first traversal"
function subexpressions(e::SExpr; subexprs = SExpr[])
    for arg in e.args
        subexpressions(arg, subexprs=subexprs)
    end
    push!(subexprs, e)
end

function size(e::SExpr) :: Float32
    size(e.head) + sum(size, e.args, init=0.)
end

function size(head::Symbol)
    1.
end

num_nodes(e::SExpr) = 1 + sum(num_nodes, e.args, init=0)

function size_no_abstraction_var(e::SExpr) :: Float32
    if startswith(string(e.head), "#")
        return 0
    end
    if isempty(e.args)
        return 1.
    end
    return size(e.head) + sum(size_no_abstraction_var, e.args)
end


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
Parse a string into an SExpr
"""
function Base.parse(::Type{SExpr}, original_s::String)
    # add guaranteed parens around whole thing and guaranteed spacing around parens so they parse into their own items
    s = replace("$original_s", "(" => " ( ", ")" => " ) ")

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