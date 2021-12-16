using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = readlines("input/day10.txt")


## part 1

function part1()
    ans = 0
    for line in input
        opened = []
        for c in line
            if c == '(' || c == '[' || c == '{' || c == '<'
                append!(opened, c)
            elseif c == ')' && opened[end] != '('
                ans += 3
                break
            elseif c == ']' && opened[end] != '['
                ans += 57
                break
            elseif c == '}' && opened[end] != '{'
                ans += 1197
                break
            elseif c == '>' && opened[end] != '<'
                ans += 25137
                break
            else
                pop!(opened)
            end
        end
        println(opened)
    end
    println(ans)
end
low_points = part1()

## part 2

function part2()
    scores = []
    for line in input
        score = 0
        opened = []
        bad = false

        for c in line
            if c == '(' || c == '[' || c == '{' || c == '<'
                append!(opened, c)
            elseif c == ')' && opened[end] != '('
                bad = true
                break
            elseif c == ']' && opened[end] != '['
                bad = true
                break
            elseif c == '}' && opened[end] != '{'
                bad = true
                break
            elseif c == '>' && opened[end] != '<'
                bad = true
                break
            else
                pop!(opened)
            end
        end

        if bad
            continue
        else
            println(line)
        end

        for c in reverse(opened)
            if c == '('
                score = score * 5 + 1
            elseif c == '['
                score = score * 5 + 2
            elseif c == '{'
                score = score * 5 + 3
            elseif c == '<'
                score = score * 5 + 4
            else
                throw("Fail")
            end
        end
        push!(scores, score)
    end
    sort!(scores)

    println(length(scores))
    println(scores[Int(ceil(end/2))])

    # 7306116 too low
    # 3498468 ? - also does not work
end
part2()