using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = [[parse(Int, c) for c in line] for line in readlines("input/day11.txt")]


## part 1

function flash(octo, x, y)
    flashes = 1

    octo[y][x] = 0
    for y2 in (y-1):(y+1)
        for x2 in (x-1):(x+1)
            if y2 < 1 || y2 > length(octo) || x2 < 1 || x2 > length(octo[x])
                continue
            end
            if octo[y2][x2] == 0
                continue
            end
            octo[y2][x2] += 1
        end
    end

    for y2 in (y-1):(y+1)
        for x2 in (x-1):(x+1)
            if y2 < 1 || y2 > length(octo) || x2 < 1 || x2 > length(octo[x])
                continue
            end
            if octo[y2][x2] > 9
                flashes += flash(octo, x2, y2)
            end
        end
    end
    
    flashes
end

function part1()
    octo = deepcopy(input)

    flashes = 0
    for iteration in 1:100
        for y in 1:length(octo)
            for x in 1:length(octo[y])
                octo[y][x] += 1
            end
        end

        for y in 1:length(octo)
            for x in 1:length(octo[y])
                if octo[y][x] > 9
                    flashes += flash(octo, x, y)
                end
            end
        end
    end

    println(flashes)
end
part1()

## part 2

function part2()    
    octo = deepcopy(input)

    for iteration in 1:10000
        flashes = 0
        for y in 1:length(octo)
            for x in 1:length(octo[y])
                octo[y][x] += 1
            end
        end

        for y in 1:length(octo)
            for x in 1:length(octo[y])
                if octo[y][x] > 9
                    flashes += flash(octo, x, y)
                end
            end
        end

        if flashes == 100
            print(iteration)
            break
        end
    end

    # 237 wrong
end
part2()