include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(s)
    v = split(s, " ")
    [v[1], parse(Int, v[2])]
end

input = readlines("input/day2.txt")
parsed = [parseInput(s) for s in input]
parsed

## part 1

function part1()
    x = 0
    y = 0

    for line in parsed
        d = line[1]
        n = line[2]
        if d == "forward"
            x += n
        elseif d == "down"
            y += n
        elseif d == "up"
            y -= n
        end
    end
    println(x * y)
end
part1()

## part 2

function part2()
    x = 0
    y = 0
    aim = 0

    for line in parsed
        d = line[1]
        n = line[2]
        if d == "forward"
            x += n
            y += aim * n
        elseif d == "down"
            aim += n
        elseif d == "up"
            aim -= n
        end
    end
    println(x * y)
end
part2()