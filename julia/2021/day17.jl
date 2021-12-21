using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)


input = [[282, 314], [-80, -45]]
# input = [[20,30], [-10,-5]]

## part 1

function part1()
    xr, yr = deepcopy(input)

    best_start = (-1, -1)
    best_y = 0

    success = 0

    # assuming target is in the positive x-dir
    for xv_start = 1:xr[2]
        for yv_start = yr[1]:1100
            xv, yv = xv_start, yv_start
            x, y = 0,0
            max_y = 0

            while x <= xr[2] && y >= yr[1]
                if xr[1] <= x <= xr[2] && yr[1] <= y <= yr[2]
                    success += 1
                    if max_y > best_y
                        best_start = (xv_start, yv_start)
                        best_y = max_y
                    end
                    break
                end

                x += xv
                y += yv

                xv = max(0, xv-1)
                yv = yv - 1

                max_y = max(max_y, y)
            end
        end
    end

    log("part 1:", best_y)
    println(best_start)

    log("part 2:", success)
end
part1()

## part 2

function part2()
end
part2()