include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    [parse(Int, i) for i in split(line, " ") if i != ""]
end

input = readlines("input/day4.txt")
nums = [parse(Int, i) for i in split(input[1], ",")]

board_blocks = Iterators.partition(input[2:end], 6) |> collect
boards = [[parseInput(line) for line in b if line != ""] for b in board_blocks]


## part 1

function iswin(board, numbers)
    s = Set(numbers)
    any([all([in(board[row][col], s) for row in 1:5]) for col in 1:5]) ||
    any([all([in(board[row][col], s) for col in 1:5]) for row in 1:5])
end

function part1()
    for offset in 1:length(nums)
        win_count = 0
        score = 0
        for board in boards
            if iswin(board, nums[1:offset])
                win_count += 1
                println("win! ", board)
                println([[in(board[row][col], nums[1:offset]) for col in 1:5] for row in 1:5])

                score = sum([!in(board[row][col], nums[1:offset]) ? board[row][col] : 0 for row in 1:5 for col in 1:5]) * nums[offset]
            end
        end

        if win_count > 0
            println(nums[1:offset])
            println(score)
            break
        end
    end
end
part1()

## part 2

function part2()
    winning_boards = Set()
    last_score = -1
    for offset in 1:length(nums)
        for board in boards
            if iswin(board, nums[1:offset]) && !in(board, winning_boards)
                push!(winning_boards, board)
                
                println("win! ", board)
                println([[in(board[row][col], nums[1:offset]) for col in 1:5] for row in 1:5])

                last_score = sum([!in(board[row][col], nums[1:offset]) ? board[row][col] : 0 for row in 1:5 for col in 1:5]) * nums[offset]
            end
        end
    end

    println(last_score)
end
part2()