using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = [4, 7]
# input = [4, 8]
# input = readlines("input/day21-test.txt")

## Part 1

r = 1
rolls = 0

function roll()
    global r, rolls
    res = r
    r = wrap(r+1, 1, 100)
    rolls += 1

    res
end

function part1()
    pos = deepcopy(input)
    scores = [0, 0]
    turn = 1

    while max(scores...) < 1000
        die_roll = roll() + roll() + roll()

        pos[turn] = wrap(pos[turn] + die_roll, 1, 10)
        scores[turn] += pos[turn]

        log(turn, r, pos[turn], scores[turn])

        turn = turn == 1 ? 2 : 1
    end

    println(rolls * min(scores...))
end
@time part1()

## part 2

function part2()
    pos = deepcopy(input)
    dp = Dict()

    WIN = 21

    s = [1,2,3]
    rolls = [x+y+z for x in s for y in s for z in s]

    function rec(turn, p1, score1, p2, score2)
        res = [0, 0]

        key = (turn, p1, score1, p2, score2)
        if haskey(dp, key)
            return dp[key]
        end

        if score1 >= WIN && score2 >= WIN
            throw("Score fail!")
        elseif score1 >= WIN
            res = [1, 0]
        elseif score2 >= WIN
            res = [0, 1]
        else
            if turn == 1
                for die in rolls
                    new_pos = wrap(p1 + die, 1, 10)
                    res += rec(2, new_pos, score1 + new_pos, p2, score2)
                end
            elseif turn == 2
                for die in rolls
                    new_pos = wrap(p2 + die, 1, 10)
                    res += rec(1, p1, score1, new_pos, score2 + new_pos)
                end
            else
                throw("Turn fail!")
            end
        end

        dp[key] = res
        return res
    end

    wins = rec(1, pos[1], 0, pos[2], 0)

    println(wins)
    println(max(wins...))
end
@time part2()