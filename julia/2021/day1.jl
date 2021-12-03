include("util.jl")

# read relative to this dir
cd(@__DIR__)
input = readlines("input/day1.txt")

nums = [parse(Int, i) for i in input]

## part 1

# soft scope problem - julia behaviour differs between repl and non-interactive :@
# in non-interactive you are not allowed to access global vars inside loops (without explicitly writing global everywhere)
# an ugly let-statement (or a function) makes it work, but this also makes it impossible to run individual statements (in vscode), beaking repl-driven development :@
# https://docs.julialang.org/en/v1/manual/variables-and-scoping/

function part1()
    prev = -1
    count = 0

    for n in nums
        if prev != -1 && n > prev
            count += 1
        end
        prev = n
    end
    println(count)
end
part1()

## part 2

function part2()
    prev = [-1,-1,-1]
    count = 0

    for n in nums
        new_prev = vcat(prev[2:3], [n])
        if prev[1] != -1 && sum(new_prev) > sum(prev)
            count += 1
        end
        prev = new_prev
    end
    println(count)
end
part2()