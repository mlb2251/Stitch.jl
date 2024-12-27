

abstract type A end
struct B <: A end
struct C <: A end

# foo(a::A) = "hi"

function foo(b::B)
    1
end

function foo(c::C)
    2
end

function foo(x::Int)
    error("foo(Int)")
end

# function randA()
#     if rand() < 0.5
#         B()
#     else
#         C()
#     end
# end

function bar(x::Any)
    # x = randA()
    y = hash(x)
    y
end