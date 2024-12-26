Base.parse(::Type{PExpr}, s) = parse_expr(s)

function parse_expr(s)
    s = replace(
        s,
        r"\(" => " ( ",
        r"\)" => " ) ",
        "{" => " { ",
        "}" => " } ",
        "->" => " -> ",
        "λ" => " λ ",
        "," => " , ",
        r"\s+" => " " # must come at end – collapse extra whitespace
    )
    tokens = split(s)
    expr, rest = parse_expr_inner(tokens, Symbol[])
    @assert isempty(rest) "parse error: trailing characters: $rest"
    return expr
end

function parse_expr_inner(tokens, env::Vector{Symbol})
    length(tokens) == 0 && error("unexpected end of input")
    token = tokens[1]
    token_sym = Symbol(token)
    if token == "("
        # Possible expression heads
        tokens = tokens[2:end]
        token = tokens[1]
        if token == "lam" || token == "lambda" || token == "λ"
            # parse (λx y z -> body) or (λx,y,z -> body) or (λ_ _ _ -> body) or (λ_ -> body)
            # semantically equivalent to (λx -> (λy -> (λz -> body)))
            tokens = tokens[2:end]
            num_args = 0
            argnames = Symbol[]
            while true
                name = tokens[1]
                @assert Base.isidentifier(name) "expected identifier for lambda argument, got $name"
                env = [name, env...]
                push!(argnames, Symbol(name))
                num_args += 1
                tokens = tokens[2:end]
                if tokens[1] == "," # optional comma
                    tokens = tokens[2:end]
                end
                if tokens[1] == "->" # end of arg list
                    tokens = tokens[2:end]
                    break
                end
            end
            body, tokens = parse_expr_inner(tokens, env)
            tokens[1] != ")" && error("expected closing paren")
            @assert false
            # return Abs(num_args, argnames, body), tokens[2:end]
        else
            # Parse an application
            f, tokens = parse_expr_inner(tokens, env)
            args = []
            while tokens[1] != ")"
                arg, tokens = parse_expr_inner(tokens, env)
                push!(args, arg)
            end
            return App(f, args), tokens[2:end]
        end
    elseif token_sym ∈ env
        # Parse a var by name like "foo"
        idx = findfirst(x -> x === token_sym, env) # shadowing
        @assert false
        # return Var(idx, token_sym), tokens[2:end]
    elseif token[1] == '#'
        # parse debruijn index #4
        idx = parse(Int, token[2:end])
        @assert idx > 0 "need one-index debruijn"
        @assert false
        # return Var(idx, get!(env, idx, NONAME)), tokens[2:end]
    elseif '#' ∈ token
        # variable combining name and debruijn like x#4
        parts = split(token, "#")
        name = Symbol(parts[1])
        idx = parse(Int, parts[2])
        @assert parts[1] == env[idx] "debruijn index must match variable name"
        @assert false
        # return Var(idx, name), tokens[2:end]
    else
        return Prim(token_sym), tokens[2:end]
    end
end


function Base.show(io::IO, e::App)
    paren_depth = get(io, :paren_depth, 0)::Int
    # printstyled(io, "("; color=paren_depth%8)
    print(io, "(")
    new_io = IOContext(io, :paren_depth => paren_depth + 1)
    print(new_io, e.f)
    for arg in e.args
        print(new_io, " ", arg)
    end
    print(io, ")")
    # printstyled(io, ")"; color=paren_depth%8)
end
# function Base.show(io::IO, e::Abs)
#     print(io, "(λ")
#     for arg in e.argnames
#         print(io, arg, " ")
#     end
#     print(io, "-> ", e.body, ")")
# end
# Base.show(io::IO, e::Var) = print(io, e.name)
Base.show(io::IO, e::Prim) = print(io, e.name)
Base.show(io::IO, e::MetaVar) = printstyled(io, "#", metavar_names[e.name]; color=(e.name%6)+1, bold=true)

