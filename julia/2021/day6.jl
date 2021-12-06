include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    s = split(line, ",")
    [parse(Int, p) for p in s]
end

input = readlines("input/day6.txt")
parsed = [parseInput(s) for s in input]
parsed


## part 1

function part1()
    fish = copy(parsed[1])
    for day in 1:80
        l = length(fish)
        for i in 1:l
            if fish[i] == 0
                fish[i] = 6
                append!(fish, 8)
            else
                fish[i] = fish[i] - 1
            end
        end
    end
    println(length(fish))
end
part1()

## part 2

function part2()
    fish = copy(parsed[1])
    counts = Dict()
    for f in fish
        counts[f] = get(counts, f, 0) + 1
    end

    for day in 1:256
        new_counts = Dict()
        for l in 0:8
            if l == 0
                fishies = get(counts, l, 0)
                new_counts[8] = fishies
                new_counts[6] = fishies
            else
                new_counts[l-1] = get(new_counts, l-1, 0) + get(counts, l, 0)
            end
        end
        counts = new_counts
    end
    println(sum(values(counts)))
end
part2()