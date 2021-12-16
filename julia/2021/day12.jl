using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    split(line, "-")
end

input = [parseInput(line) for line in readlines("input/day12.txt")]

paths = Dict()
for line in input
    (a, b) = line
    paths[a] = push!(get(paths, a, []), b)
    paths[b] = push!(get(paths, b, []), a)
end
paths

## part 1

function dfs(node, seen)
    if node == "end"
        return 1
    end

    if node == lowercase(node)
        push!(seen, node)
    end

    count = 0
    for neigh in paths[node]
        if in(neigh, seen)
            continue
        end
        count += dfs(neigh, seen)
    end
    delete!(seen, node)

    count
end

function part1()
    seen = Set(["start"])
    count = dfs("start", seen)
    println(count)
end
part1()

## part 2

function dfs2(node, double_visit, path)
    if length(path) > 20
        return 0
    end

    if node == "end"
        println(join(path, " -> "), " -> end")
        return 1
    end

    push!(path, node)

    count = 0
    for neigh in paths[node]
        if neigh == "start"
            continue
        end

        if neigh == lowercase(neigh) && in(neigh, path)
            if double_visit == false
                count += dfs2(neigh, true, path)
            end
        else
            count += dfs2(neigh, double_visit, path)
        end
    end

    pop!(path)
    count
end

function part2()    
    count = dfs2("start", false, [])
    println(count)
end
part2()