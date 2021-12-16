using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    s = split(line, "|")
    ss = [strip(p) for p in s]
    [split(p, " ") for p in ss]
end

input = readlines("input/day8.txt")
parsed = [parseInput(s) for s in input]
parsed


## part 1

function part1()
    ans = 0
    for line in parsed
        for d in line[2]
            l = length(d)
            if l == 2 || l == 4 || l == 3 || l == 7
                ans += 1
            end
        end
    end
    println(ans)
end
part1()

# 1: cf (2)
# 4: bcdf (4)
# 7: acf (3)
# 8: abcdefg (7)

# 0, 6, 9: (6)
# 2, 3, 5: (5)

# interesting: d, c, e
# b/f, b/e, c/e

# a = in 7 but not in 1
# f = in 7, 4 and 1 - (true for c and f)
# b = in 7 but not in 4

# n5 = five digit, does have b
# n3 = five digit, subset of 1
# n2 = five digit, does have f

## part 2

function getfirst(arr)
    if length(arr) > 1
        throw(string("ambigious", arr))
    end
    return arr[1]
end

# 0, 6, 9: (6)
# 2, 3, 5: (5)

function part2()
    ans = 0

    for line in parsed
        alph = "abcdefg"
        signals = line[1]

        n1 = getfirst([l for l in signals if length(l) == 2])
        n4 = getfirst([l for l in signals if length(l) == 4])
        n7 = getfirst([l for l in signals if length(l) == 3])
        n8 = getfirst([l for l in signals if length(l) == 7])

        n3 = getfirst([l for l in signals if length(l) == 5 && issubset(n1, l) ])

        n9 = getfirst([l for l in signals if length(l) == 6 && issubset(n3, l) ])
        n0 = getfirst([l for l in signals if length(l) == 6 && l != n9 && issubset(n1, l) ])
        n6 = getfirst([l for l in signals if length(l) == 6 && l != n9 && !issubset(n1, l) ])

        n5 = getfirst([l for l in signals if length(l) == 5 && l != n3 && issubset(l, n9) ])
        n2 = getfirst([l for l in signals if length(l) == 5 && l != n3 && l != n5 ])

        translate = Dict(
            Set(n0) => 0,
            Set(n1) => 1,
            Set(n2) => 2,
            Set(n3) => 3,
            Set(n4) => 4,
            Set(n5) => 5,
            Set(n6) => 6,
            Set(n7) => 7,
            Set(n8) => 8,
            Set(n9) => 9,
        )

        value = 0
        for d in line[2]
            value = value * 10
            value += translate[Set(d)]
        end

        ans += value
    end

    println(ans)
end
part2()