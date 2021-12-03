## Julia language

### This is executable as Julia code

## Ints

    # power, not xor
    3^4
    # weird xor operator
    3⊻4
    xor(3, 4)

    1 << 12

    parse(Int, "123")
    parse(Int, "1000", base=2)
    0b1000

## Strings

    string("Hello", " ", "World")
    join(["Hello", "World"], ", ")

    string(123)
    join(digits(8, base=2) |> reverse)

## Lists

    l1 = [1, 2, 3]
    l2 = [i+1 for i in l1]
    l3 = [i for i in l2 if i%2 == 0]

    # Are one indexed!!
    l1[3]

## Sorting

    sort([1,3,2], rev=true)

    sort(["aaa","bb","c"], by=x->length(x))

## Dicts ??

    d = Dict(1=>2, 3=>4)
    # get that throws or get with default
    d[1] = 2
    get(d, 2, -1)

## Functions

    123 |> string |> length

## Exceptions

    throw(DomainException("something failed"))