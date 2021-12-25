using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = readlines("input/day22.txt")
# input = readlines("input/day22-test2.txt")

function parseLine(line)
    onoff, pos = split(line, " ")
    vcat([onoff == "on"], [parsePos(p) for p in split(pos, ",")])
end

function parsePos(pos)
    p2 = split(pos, "=")[2]
    [parse(Int, p) for p in split(p2, "..")]
end

parsed = [parseLine(line) for line in input]

## Part 1

function part1()
    function size(range)
        range[2] - range[1]
    end

    d = Dict()

    for p in parsed
        on = p[1]
        for x in cap(p[2][1], -50, 51):cap(p[2][2], -51, 50)
            for y in cap(p[3][1], -50, 51):cap(p[3][2], -51, 50)
                for z in cap(p[4][1], -50, 51):cap(p[4][2], -51, 50)
                    d[(x,y,z)] = on
                end
            end
        end
    end

    count = 0
    for x in -50:50
        for y in -50:50
            for z in -50:50
                if get(d, (x,y,z), false)
                    count += 1
                end
            end
        end
    end
    println(count)
end
@time part1()

## part 2

function overlap(cube1, cube2)
    overlapAxis(cube1[2], cube2[2]) &&
    overlapAxis(cube1[3], cube2[3]) &&
    overlapAxis(cube1[4], cube2[4])
end

function overlapAxis(axis1, axis2)
    !(axis1[2] < axis2[1] || axis1[1] > axis2[2])
end

function inside(cube1, cube2)
    insideAxis(cube1[2], cube2[2]) &&
    insideAxis(cube1[3], cube2[3]) &&
    insideAxis(cube1[4], cube2[4])
end

function insideCount(cube1, cube2)
    (insideAxis(cube1[2], cube2[2]) ? 1 : 0) + 
    (insideAxis(cube1[3], cube2[3]) ? 1 : 0) +
    (insideAxis(cube1[4], cube2[4]) ? 1 : 0)
end

function insideAxis(axis1, axis2)
    axis1[1] >= axis2[1] && axis1[2] <= axis2[2]
end

function part2()
    overlap_count = BigInt(0)
    overridden_count = BigInt(0)
    subcube_count = BigInt(0)

    parent_counts = [0,0,0,0]
    child_counts = [0,0,0,0]

    full_override = []

    for i in 1:length(parsed)
        p = parsed[i]
        for j in i+1:length(parsed)
            p2 = parsed[j]
            if overlap(p, p2)
                overlap_count += 1

                parent_counts[insideCount(p, p2) + 1] += 1
                child_counts[insideCount(p2, p) + 1] += 1
            end
            if inside(p, p2)
                overridden_count += 1
                # these cubes can be ignored
                push!(full_override, i)
            end
            if inside(p2, p)
                subcube_count += 1
            end
        end
    end

    log("overlap", overlap_count)
    log("fully overridden", overridden_count)
    log("fully inside", subcube_count)
    log("side inside counts", parent_counts, child_counts)

    # overlap count 1825
    # splitting cube in 26 parts - 30638224304653281318 splits :/

    # actually solve it

    function cubeSize(cube)
        x, y, z = cube
        if x[2] < x[1] || y[2] < y[1] || z[2] < z[1]
            return 0
        end
        return (x[2] - x[1] + 1) * (y[2] - y[1] + 1) * (z[2] - z[1] + 1)
    end

    function cubeIntersect(cube1, cube2)
        new_cube = [
            [max(cube1[1][1], cube2[1][1]), min(cube1[1][2], cube2[1][2])],
            [max(cube1[2][1], cube2[2][1]), min(cube1[2][2], cube2[2][2])],
            [max(cube1[3][1], cube2[3][1]), min(cube1[3][2], cube2[3][2])],
        ]
        if cubeSize(new_cube) == 0
            return nothing
        end
        new_cube
    end

    println(cubeIntersect([[1,1], [1,1], [1,1]], [[1,1], [1,1], [1,1]]))
    println(cubeIntersect([[1,1], [1,1], [1,1]], [[1,2], [1,2], [1,2]]))
    println(cubeIntersect([[1,2], [1,2], [1,2]], [[2,3], [2,3], [2,3]]))

    function cubeSizeSum(cubes)
        ret = 0

        for i in 1:length(cubes)
            c1 = cubes[i]
            overlap = []
            for j in i+1:length(cubes)
                co = cubeIntersect(c1, cubes[j])
                if co !== nothing
                    push!(overlap, co)
                end
            end
            ret += cubeSize(c1) - cubeSizeSum(overlap)
        end
        ret
    end

    println(cubeSizeSum([[[1,1],[1,1],[1,1]]]))
    println(cubeSizeSum([
        [[1,1],[1,1],[1,1]], 
        [[2,2],[2,2],[2,2]], 
        [[1,3],[1,3],[1,3]], 
        [[1,4],[1,1],[1,1]], 
    ]))

    function finalSize(cube, othercubes)
        overlap = []
        for other in othercubes
            co = cubeIntersect(cube, other)
            if co !== nothing
                push!(overlap, co)
            end
        end
        cubeSize(cube) - cubeSizeSum(overlap)
    end

    log("finalSize", finalSize(
        [[1,4], [1,3], [1,3]]
    , [
        [[1,1],[1,1],[1,1]], 
        [[2,2],[2,2],[2,2]], 
        [[1,3],[1,3],[1,3]], 
        [[1,4],[1,1],[1,1]], 
    ]))

    switch = [l[1] for l in parsed]
    cubes = [l[2:4] for l in parsed]

    count = 0
    for i in 1:length(parsed)
        cube = cubes[i]
        if switch[i] == true
            size = finalSize(cube, cubes[i+1:end])
            count += size
        end
    end
    println(count)

    # TODO BigInt needed?
end
@time part2()
