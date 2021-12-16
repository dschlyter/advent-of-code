using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    s = split(line, ",")
    [parse(Int, p) for p in s]
end

input = readlines("input/day7.txt")
parsed = [parseInput(s) for s in input]
parsed


## part 1

function part1()
    crabs = parsed[1]
    best_fuel = sum(crabs)
    for pos in min(crabs...):max(crabs...)
        fuel = 0
        for c in crabs
            fuel += abs(c - pos)
        end
        best_fuel = min(best_fuel, fuel)
    end
    println(best_fuel)
end
part1()

## part 2

length(parsed[1])

function part2()
    crabs = parsed[1]
    best_fuel = sum(crabs)*sum(crabs)
    for pos in min(crabs...):max(crabs...)
        fuel = 0
        for c in crabs
            dist = abs(c - pos)
            s = (dist+1)*dist/2 
            fuel += s
        end
        best_fuel = min(best_fuel, fuel)
    end
    @printf "%d" best_fuel
end
part2()