include("util.jl")

# read relative to this dir
cd(@__DIR__)
input = readlines("input/day1.txt")

nums = [parse(Int, i) for i in input]

for i in 1:length(nums)
    for j in i+1:length(nums)
        if nums[i]+nums[j] == 2020
            print(nums[i] * nums[j])
        end
    end
end