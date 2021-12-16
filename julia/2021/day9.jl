using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    [parse(Int, c) for c in line]
end

input = readlines("input/day9.txt")
parsed = [parseInput(s) for s in input]
parsed


## part 1

function part1()
    g = parsed
    danger = 0
    low_points = [] # Array{Tuple{Int, Int}, 1}() - not working
    for y in 1:length(g)
        for x in 1:length(g[y])
            p = g[y][x]
            if x > 1 && g[y][x-1] <= p
                continue
            elseif x < length(g[y]) && g[y][x+1] <= p
                continue
            elseif y > 1 && g[y-1][x] <= p
                continue
            elseif y < length(g) && g[y+1][x] <= p
                continue
            end
            danger = danger + 1 + p
            push!(low_points, (x, y))
        end
    end
    println(danger)
    low_points
end
low_points = part1()

## part 2

g = parsed

function dfs(x, y, seen)
    if y < 1 || y > length(g) || x < 1 || x > length(g[y])
        return 0
    end
    if in((x,y), seen)
        return 0
    end
    if g[y][x] == 9
        return 0
    end
    push!(seen, (x,y))

    acc = 1
    acc += dfs(x-1, y, seen)
    acc += dfs(x+1, y, seen)
    acc += dfs(x, y-1, seen)
    acc += dfs(x, y+1, seen)
    return acc
end

function part2()
    basins = []
    seen = Set()
    for (x,y) in low_points
        basin_size = dfs(x, y, seen)
        push!(basins, basin_size)
    end
    sort!(basins, rev=true)
    println(basins)
    println(basins[1] * basins[2] * basins[3])
end
part2()