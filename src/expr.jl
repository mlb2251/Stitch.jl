
"""
An expression. Contains parent pointer, so deepcopying will include all parents if you don't set that reference
to `nothing` first - see `detach_deepcopy`.
"""
mutable struct SExpr
    head::Symbol
    args::Vector{SExpr}
    parent::Union{SExpr, Nothing}

    # type annotation on args forces the more efficient passing in of a SExpr vector
    function SExpr(head, args::Vector{SExpr}, parent)
        expr = new(head, args, parent)
        for arg in args
            arg.parent = expr
        end
        expr
    end
end

new_hole(parent) = SExpr(Symbol("??"), SExpr[], parent)

function detach_deepcopy(e::SExpr) :: SExpr
    e.parent = nothing
    deepcopy(e)
end

function subexpressions(e::SExpr; subexprs = SExpr[]) :: Vector{SExpr}
    for arg in e.args
        subexpressions(arg, subexprs=subexprs)
    end
    push!(subexprs, e)
end

function size(e::SExpr) :: Float32
    total = 1
    if length(e.args) == 0
        return 1
    end
    return .01 + sum(size, e.args)
end

function size_no_abstraction_var(e::SExpr) :: Float32
    if startswith(string(e.head), "#")
        return 0
    end
    if length(e.args) == 0
        return 1
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
function uncurry(e::SExpr) :: Vector{SExpr}
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
                node = SExpr(:app, [node, items[end-item_count+i]], nothing)
            end
            items = items[1:end-item_count]

            push!(items, node)
            item_count = pop!(item_counts)
            item_count += 1
        else
            push!(items, SExpr(Symbol(item), SExpr[], nothing))
            item_count += 1
        end
    end

    depth == 0 || error("unbalanced parens: not enough close parens")

    item_count == length(items) || error("item_count != length(items)")

    node = items[1]
    for i in 2:item_count
        node = SExpr(:app, [node, items[i]], nothing)
    end

    return node
end