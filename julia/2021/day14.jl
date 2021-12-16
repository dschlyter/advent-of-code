using Printf

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput2(line)
    s = split(line, " -> ")
    (s[1], s[2])
end

input = readlines("input/day14.txt")

seq = input[1]
inserts = Dict([parseInput2(line) for line in input[3:end]])

## part 1

function part1()
    s = deepcopy(seq)

    for i in 1:10
        i = 1
        while i < length(s) 
            pair = s[i:i+1]
            c = get(inserts, pair, nothing)
            if c !== nothing
                s = string(s[1:i], c, s[i+1:end])
                i += 1
            end
            i += 1
        end
    end

    println(s)
    cv = count_values(s)
    most_common = sort([reverse(x) for x in cv], rev=true)[1][1]
    least_common = sort([reverse(x) for x in cv])[1][1]
    println(most_common - least_common)
end
part1()

## part 2

memo = Dict()

function expand(pair, times)
    if times == 0
        return Dict()
    end

    key = (pair, times)
    mem = get(memo, key, nothing)
    if mem !== nothing
        return mem
    end

    c = get(inserts, pair, nothing)
    if c !== nothing
        a = expand(string(pair[1], c), times-1)
        b = expand(string(c, pair[2]), times-1)
        ret = merge(+, Dict(c => 1), a, b)
    else
        ret = Dict()
    end

    memo[key] = ret
    ret
end

function part2()    
    counts = count_values([string(x) for x in seq])
    println(counts)
    for i in 1:length(seq)-1
        pair = seq[i:i+1]
        counts = merge(+, counts, expand(pair, 40))
    end

    println(counts)
    most_common = sort([reverse(x) for x in counts], rev=true)[1][1]
    least_common = sort([reverse(x) for x in counts])[1][1]
    println(most_common - least_common)
end
part2()