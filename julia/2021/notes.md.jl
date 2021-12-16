## Julia language

### This is executable as Julia code

## null

    nothing

    # julia objects are non-nullable by default

## Ints

    parse(Int, "123")
    parse(Int, "1000", base=2)
    0b1000

    # power, not xor
    3^4

    # weird xor operator
    3⊻4
    xor(3, 4)

    1 << 12

    # operators are functions
    1 + 2
    +(1,2,3)
    f = +
    f(1,2,3)

## Strings

    string("Hello", " ", "World")
    join(["Hello", "World"], ", ")

    string(123)
    join(digits(8, base=2) |> reverse)

    "regular strings can have $varaibles"

## Regex

    occursin(r"Hello [0-9]+", "Hello 123")

## Version number type

    v"12.0" > v"9"

## Vector

    l1 = [1, 2, 3]
    l2 = [i+1 for i in l1]
    l3 = [i for i in l2 if i%2 == 0]

    l4 = copy(l3)
    push!(l4, 100)
    append!(l4, [1,2,3])

    # concat
    [l1; l2]

    # Are one indexed!!
    l1[3]

    [1,2,3,4][1:2]

    # last value
    [1,2,3,4][end]

    # broadcast operations
    [1,2,3] .+ 5

    # matrix with columns
    [[1,2,3] [4,5,6]]

    # matrix with rows
    [[1 2 3];; [4 5 6]]

## Sorting

    sort([1,3,2], rev=true)

    sort(["aaa","bb","c"], by=x->length(x))

## Dicts ??

    d = Dict(1=>2, 3=>4)
    # get that throws or get with default
    d[1] = 2
    get(d, 2, -1)

## Sets

    s = Set([1,2,3])
    push!(s, 4)
    in(4, s)
    union(s, Set([4,5]))
    intersect(s, s)
    setdiff(s, s)
    issubset(s, s)

## Tuples and named tuples

    (1, 2)
    (a=1, b=2)

## Functions

    # lambda
    (x -> x * 2)(1)

    # shorthand declaration
    fx(x) = x+1
    fx(2)

    # chaining
    123 |> string |> length

    # dot chaining
    [1,2,3] .|> sqrt

    # vectorizing (same result as above)
    sqrt.([1,2,3])

    # composition with weird operator
    (sqrt ∘ +)(3, 6)

    # One function has multiple methods, using multiple dispatch on arguments
    methods(+)

## Varargs

    fv(a, v...) = (a, v)
    fv(1,2,3,4,5)

    fv(1, [2,3,4,5]...)

## Structs

    struct Foo
        bar
        baz::Int
        qux::Float64
    end
    f = Foo(1, 2, 3.0)
    fieldnames(Foo)

## Create constructors by defining new methods

    Foo() = Foo(1, 2, 4.0)
    Foo()

## Optionals

    struct Maybe
        x::Union{Int, Nothing}
    end
    Maybe(2)
    Maybe(nothing)

## Exceptions

    throw(DomainException("something failed"))

    error("wat!?")

## Async

    t = @task begin; sleep(5); println("done"); end
    schedule(t); wait(t)

     # Julia has channels for comms
    c = Channel{Int}()
    schedule(@task begin; sleep(5); println(take!(c)); end)
    put!(c, 42)


## Math

# A lot of functions are defined sqrt, sin, atan(1,0)

    # complex numbers
    5 + 1im

    # rationals
    numerator(2 // 3)

# Packages

    # Can be installed inline in the code
    using Pkg
    Pkg.add("Packagename")

## Priority queue - kinda weird since values set the priority (not the key)

    using Pkg
    Pkg.add("DataStructures"); 
    using DataStructures;

    # keys must be unique, and there is no 
    id = 0

    # enqueue but skip if existing and lower
    function enq(q, k, v)
        q[k] = min(get(q, k, v), v)
    end

    q = PriorityQueue()
    enq(q, "first value", 10)
    enq(q, "second value", 2)
    enq(q, "first value", 1)

    v1, v2 = dequeue!(q), dequeue!(q)

