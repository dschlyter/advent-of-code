using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = readlines("input/day25.txt")
# input = readlines("input/day25-test2.txt")

p = [[c for c in line] for line in input]

## part 1

function nxt(i, coll)
    (i - 1 + 1) % length(coll) + 1
end

function part1()
    cu = deepcopy(p)

    count = 0
    moved = true
    while moved
        moves = Set()
        for y in 1:length(cu)
            for x in 1:length(cu[y])
                if cu[y][x] != '>'
                    continue
                end
                if cu[y][nxt(x, cu[y])] == '.'
                    push!(moves, (y,x))
                end
            end
        end
        for (y,x) in moves
            cu[y][x] = '.'
            cu[y][nxt(x, cu[y])] = '>'
        end
        moved = length(moves) > 0

        moves = Set()
        for y in 1:length(cu)
            for x in 1:length(cu[y])
                if cu[y][x] != 'v'
                    continue
                end
                if cu[nxt(y, cu)][x] == '.'
                    push!(moves, (y,x))
                end
            end
        end
        for (y,x) in moves
            cu[y][x] = '.'
            cu[nxt(y, cu)][x] = 'v'
        end
        moved = moved || length(moves) > 0

        count += 1
        println(count)
    end
end
@time part1()

# --- attempt 2
