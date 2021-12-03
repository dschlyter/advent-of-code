include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(s)
    s
end

input = readlines("input/day3.txt")
parsed = [parseInput(s) for s in input]
parsed


## part 1

function part1()
    word = ""
    for pos in 1:length(parsed[1])
        bit = mode([v[pos] for v in parsed])
        word = string(word, bit)
    end

    println(word)
    gamma = parse(Int, word, base=2)
    epsilon = xor((1 << length(word) - 1), gamma)
    println(gamma, " ", epsilon)
    println(gamma * epsilon)

    return word
end
part1()

## part 2

function part2()
    function find(criteria)
        cand = parsed

        for pos in 1:length(parsed[1])
            if length(cand) == 1
                break
            end

            counts = count_values([v[pos] for v in cand])
            if get(counts, '1', 0) >= get(counts, '0', 0)
                bit = '1'
            else
                bit = '0'
            end

            next_cand = []
            for v in cand
                if (v[pos] == bit) == criteria
                    append!(next_cand, [v])
                end
            end
            cand = next_cand
        end

        cand[1]
    end

    oxygen_w = find(true)
    co2_w = find(false)

    println(oxygen_w, ", ", co2_w)

    oxygen = parse(Int, oxygen_w, base=2)
    co2 = parse(Int, co2_w, base=2)
    println(oxygen, " ", co2)
    println(oxygen * co2)
end
part2()