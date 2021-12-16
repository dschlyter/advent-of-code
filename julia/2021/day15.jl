using Printf

using Pkg
Pkg.add("DataStructures"); 
using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput2(line)
    [parse(Int, p) for p in line]
end

input = readlines("input/day15.txt")
# input = readlines("input/day15-test.txt")

parsed = [parseInput2(line) for line in input]

## part 1

function findpath(g)
    ans = 0

    visited = Set()
    q = [(-g[1][1], (1,1))]
    while length(q) > 0
        best = q[1]
        q = q[2:end]

        (y, x) = best[2]

        if x < 1 || y < 1 || y > length(g) || x > length(g[y]) || in((y,x), visited)
            continue
        end
        push!(visited, (y,x))

        cost = best[1] + g[y][x]

        if y == length(g) && x == length(g[y])
            ans = cost
            break
        end

        append!(q, [(cost, (y-1, x)), (cost, (y+1, x)), (cost, (y, x-1)), (cost, (y, x+1))])
        # super ugly priority queue
        sort!(q)
    end

    println(ans)
end

findpath(parsed)

## part 2

function findpath2(g)
    ans = 0
    id = 0

    q = PriorityQueue()
    visited = Set()

    enqueue!(q, (1, 1), -g[1][1])
    while length(q) > 0
        pos, cost = peek(q)
        dequeue!(q)

        (y, x) = pos

        if x < 1 || y < 1 || y > length(g) || x > length(g[y]) || in((y,x), visited)
            continue
        end
        push!(visited, (y,x))

        cost += g[y][x]

        if y == length(g) && x == length(g[y])
            ans = cost
            break
        end

        # enqueue but skip if existing and lower
        function enc(q, k, v)
            q[k] = min(get(q, k, v), v)
        end

        enc(q, (y-1, x), cost)
        enc(q, (y+1, x), cost)
        enc(q, (y, x-1), cost)
        enc(q, (y, x+1), cost)
    end

    println(ans)
end

g = deepcopy(parsed)

# append vertically
for i in 1:4
    append!(g, [[(x-1+i) % 9 + 1 for x in line] for line in parsed])
end

# append horizontally
for y in 1:length(g)
    line = deepcopy(g[y])
    for i in 1:4
        append!(g[y], [(x-1+i) % 9 + 1 for x in line])
    end
end

findpath2(g)