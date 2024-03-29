using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = readlines("input/day19.txt")
# input = readlines("input/day19-test.txt")

scanners = []
for line in input
    if line == ""
        continue
    end
    if startswith(line, "---")
        push!(scanners, [])
    else
        push!(scanners[end], [parse(Int, n) for n in split(line, ",")])
    end
end
println(scanners)

## Part 1

id_matrix = [1 0 0; 0 1 0; 0 0 1]

rot_x = [1 0 0; 0 0 1; 0 -1 0]
rot_y = [0 0 1; 0 1 0; -1 0 0]
rot_z = [0 -1 0; 1 0 0; 0 0 1]

rotations = Set([
    rx * ry * rz 
    for rx in [id_matrix, rot_x, rot_x*rot_x, rot_x*rot_x*rot_x]
    for ry in [id_matrix, rot_y, rot_y*rot_y, rot_y*rot_y*rot_y]
    for rz in [id_matrix, rot_z, rot_z*rot_z, rot_z*rot_z*rot_z]
])

function dist(x, y)
    return abs(x[1] - y[1]) + abs(x[2] - y[2]) + abs(x[3] - y[3])
end

function translate(points, vec)
    Set([p + vec for p in points])
end

function rot_points(points, rot)
    Set([transpose(transpose(p) * rot) for p in points])
end

function part1()
    points = [p for p in scanners[1]]
    included = Set([1])
    tested = Set()
    scanner_pos = Set([[0,0,0]])

    for i in 1:length(scanners)
        if length(included) == length(scanners)
            break
        end

        original_points = points

        for p1 in points
            if in(p1, tested)
                continue
            end
            # we can stop testing if we only have 11 points left. mark all remaining as tested.
            if length(tested) >= length(original_points) - 11
                push!(tested, p1)
                continue
            end
            push!(tested, p1)

            for oi in 1:length(scanners)
                if in(oi, included)
                    continue
                end

                other_scanner = scanners[oi]
                rotated_points = [rot_points(other_scanner, rot) for rot in rotations]

                for other_points in rotated_points
                    test_count = 0
                    for p2 in other_points
                        # less than optimal skip
                        if oi in included
                            break
                        end

                        # opt, since 12 points need to match, you don't need to test all points
                        if test_count >= length(other_points) - 11
                            break
                        end
                        test_count += 1

                        offset = p1 - p2
                        ost = translate(other_points, offset)
                        matched = length(intersect(points, ost))
                        if matched >= 12
                            println("ost! ", oi, " ", matched)
                            # search through new points first, should be faster
                            points = union(points, ost)
                            push!(scanner_pos, -offset)
                            push!(included, oi)
                        end
                    end
                end
            end
        end
    end

    println("segments: ", length(included))
    println("length: ", length(points))

    scanner_pos
end
scan_pos = @time part1()

# 176 seconds execution time, with tested-set opt
# 120s with p1 in the outer loop
# 15s with 12 point check opt


## part 2

function part2()
    s = [p for p in scan_pos]
    ans = 0
    for i in 1:length(scan_pos)
        for j in i+1:length(scan_pos)
            ans = max(ans, dist(s[i], s[j]))
        end
    end
    println(ans)
end
@time part2()