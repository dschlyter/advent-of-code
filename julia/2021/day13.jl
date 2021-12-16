using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    [parse(Int, p) for p in split(line, ",")]
end

function parseInput2(line)
    s = split(line, " ")
    s2 = split(s[3],"=")
    [s2[1], parse(Int, s2[2])]
end

input = readlines("input/day13.txt")

coords = [parseInput(line) for line in input if occursin(",", line)]
folds = [parseInput2(line) for line in input if occursin("=", line)]

## part 1

function part1()
    nc = coords
    println("original length: ", length(nc))

    for f in folds
        fc = f[2]
        nc2 = []

        if f[1] == "x"
            for c in nc
                if fc == c[1]
                    println("on fold")
                    continue
                end
                if c[1] > fc
                    push!(nc2, [2*fc - c[1], c[2]])
                else
                    push!(nc2, c)
                end
            end
        end

        if f[1] == "y"
            for c in nc
                if fc == c[2]
                    println("on fold")
                    continue
                end
                if c[2] > fc
                    push!(nc2, [c[1], 2*fc - c[2]])
                else
                    push!(nc2, c)
                end
            end
        end

        nc = nc2

        println(length(Set(nc)))
    end

    return nc
end
folded = part1()

## part 2

function dfs2(node, double_visit, path)
end

function part2()    
    s = Set(folded)
    println(s)
    for y in 0:6
        for x in 0:40
            if in([x, y], s)
                print("x")
            else
                print(" ")
            end
        end
        println()
    end
end
part2()