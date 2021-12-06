include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    s = split(line, " ")
    parsePoint(s[1]), parsePoint(s[3])
end

function parsePoint(point)
    s = split(point, ",")
    parse(Int, s[1]), parse(Int, s[2])
end

input = readlines("input/day5.txt")
parsed = [parseInput(s) for s in input]
parsed


## part 1

function part1()
    marked = Dict()
    for line in parsed
        ((x1, y1), (x2, y2)) = line
        if x1 == x2
            x = x1
            for y in min(y1,y2):max(y1,y2)
                marked[(x, y)] = get(marked, (x, y), 0) + 1
            end
        elseif y1 == y2
            y = y1
            for x in min(x1,x2):max(x1,x2)
                marked[(x, y)] = get(marked, (x, y), 0) + 1
            end
        end
    end
    c = count(p -> p >= 2, values(marked))
    println(c)
end
part1()

## part 2

function any_range(p1, p2)
    if p1 <= p2
        p1:p2
    else
        reverse(p2:p1)
    end
end

function part2()
    marked = Dict()
    for line in parsed
        ((x1, y1), (x2, y2)) = line
        if x1 == x2
            x = x1
            for y in min(y1,y2):max(y1,y2)
                marked[(x, y)] = get(marked, (x, y), 0) + 1
            end
        elseif y1 == y2
            y = y1
            for x in min(x1,x2):max(x1,x2)
                marked[(x, y)] = get(marked, (x, y), 0) + 1
            end
        else
            points = zip(any_range(x1, x2), any_range(y1,y2))
            for (x, y) in points
                marked[(x, y)] = get(marked, (x, y), 0) + 1
            end
        end
    end
    c = count(p -> p >= 2, values(marked))
    println(c)

    # 23463 too high

end
part2()