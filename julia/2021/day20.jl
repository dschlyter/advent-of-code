using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = readlines("input/day20.txt")
# input = readlines("input/day20-test.txt")

algo = input[1]

lines = input[3:end]

println(lines[end])

## Part 1

length(algo)

function get_index(board, pos, default)
    (y, x) = pos

    ret = 0
    for p in [
        (y-1, x-1), (y-1, x), (y-1, x+1),
        (y, x-1), (y, x), (y, x+1),
        (y+1, x-1), (y+1, x), (y+1, x+1),
    ]
        ret = ret * 2
        if get(board, p, default) == '#'
            ret += 1
        end 
    end
    ret
end

function run(times)
    board = Dict()
    for y in 1:length(lines)
        for x in 1:length(lines[y])
            board[(y,x)] = lines[y][x] == '#' ? '#' : '.'
        end
    end

    default = '.'

    for run in 1:times
        miny = min([p[1] for p in keys(board)]...) - 1
        maxy = max([p[1] for p in keys(board)]...) + 1
        minx = min([p[2] for p in keys(board)]...) - 1
        maxx = max([p[2] for p in keys(board)]...) + 1

        new_board = Dict()
        for y in miny:maxy
            for x in minx:maxx
                i = get_index(board, (y,x), default)
                new_board[(y,x)] = algo[i+1]
            end
        end
        board = new_board

        default_index = default == '.' ? 1 : 512
        default = algo[default_index]
    end

    # vis
    miny = min([p[1] for p in keys(board)]...) - 1
    maxy = max([p[1] for p in keys(board)]...) + 1
    minx = min([p[2] for p in keys(board)]...) - 1
    maxx = max([p[2] for p in keys(board)]...) + 1

    for y in miny:maxy
        for x in minx:maxx
            print(get(board, (y,x), default))
        end
        println()
    end

    count = length([cell for cell in values(board) if cell == '#'])
    println(count)
end

function part1()
    run(2)
end
@time part1()

## part 2

function part2()
    run(50)
    # 5057 too low
end
@time part2()