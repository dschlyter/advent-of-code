include("util.jl")

## this is a code cell - press alt-enter to run it!
# (or press shift enter to run one line)
println("code")
println("cell")
parse(Int, "2") + 3
##

# read from stdin
# input = readlines()
# read relative to this dir
cd(@__DIR__)
input = readlines("input/day1.txt")

# just learning the language
# one indexed and inclusive ranges :/
size(input)
length(input)
input[1:2]
input[5:length(input)]
[string(i) for i in 1:10]

nums = [parse(Int, i) for i in input]

for i in 1:length(nums)
    for j in i+1:length(nums)
        if nums[i]+nums[j] == 2020
            print(nums[i] * nums[j])
        end
    end
end

for i in 1:length(nums)
    for j in i+1:length(nums)
        for k in j+1:length(nums)
            if nums[i]+nums[j]+nums[k] == 2020
                println(nums[i] * nums[j] * nums[k])
            end
        end
    end
end