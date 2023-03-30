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

function subexpressions(e::SExpr; subexprs = SExpr[])
    for arg in e.args
        subexpressions(arg, subexprs=subexprs)
    end
    push!(subexprs, e)
end

function size(e::SExpr) :: Float32
    if isempty(e.args) 1. else .01 + sum(size, e.args) end
end

function size_no_abstraction_var(e::SExpr) :: Float32
    if startswith(string(e.head), "#")
        return 0
    end
    if isempty(e.args)
        return 1.
    end
    return .01 + sum(size_no_abstraction_var, e.args)
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


function Base.parse(::Type{SExpr}, original_s::String)
    # add guaranteed spacing around parens so they parse into their own items
    s = replace(original_s, "(" => " ( ", ")" => " ) ")

    items = SExpr[]
    item_counts = Int[]
    item_count = 0
    depth = 0

    # `split` will skip all quantities of all forms of whitespace
    for item in split(s)
        if item == "("
            depth += 1
            push!(item_counts, item_count)
            item_count = 0
        elseif item == ")"
            depth -= 1
            if depth < 0
                error("unbalanced parens: too many close parens")
            end

            node = items[end-item_count+1]
            for i in 2:item_count
                node = SExpr(:app, args=[node, items[end-item_count+i]])
            end
            items = items[1:end-item_count]

            push!(items, node)
            item_count = pop!(item_counts)
            item_count += 1
        else
            push!(items, SExpr(Symbol(item)))
            item_count += 1
        end
    end

    depth == 0 || error("unbalanced parens: not enough close parens")

    item_count == length(items) || error("item_count != length(items)")

    node = items[1]
    for i in 2:item_count
        node = SExpr(:app, [node, items[i]])
    end

    return node
end