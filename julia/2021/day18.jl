using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput(line)
    # DANGER: Eval input as julia code
    return eval(Meta.parse(line))
end

input = readlines("input/day18.txt")
# input = readlines("input/day18-test.txt")
p = [parseInput(line) for line in input]

mutable struct Tree2
    value
    left
    right
    up
end

function parseTree(lp)
    if lp === nothing
        return nothing
    elseif isa(lp, Array)
        l = parseTree(lp[1])
        r = parseTree(lp[2])
        t = Tree2(nothing, l, r, nothing)
        l.up = t
        r.up = t
        t
    else
        Tree2(lp, nothing, nothing, nothing)
    end
end

trees = [parseTree(line) for line in p]

function printTree(t)
    printTreeRec(t)
    println()
end
function printTreeRec(t)
    if t === nothing
        return
    end
    if t.value !== nothing
        print(t.value)
    end
    if t.left !== nothing || t.right !== nothing
        print("[")
        printTreeRec(t.left)
        print(",")
        printTreeRec(t.right)
        print("]")
    end
end
printTree(trees[1])

function p2(line)
    parseTree(parseInput(line))
end

## part 1

function first(t)
    if t.left !== nothing
        first(t.left)
    elseif t.value !== nothing
        t
    end
end

function last(t)
    if t.right !== nothing
        last(t.right)
    elseif t.value !== nothing
        t
    end
end

function prevValue(t)
    if t.left !== nothing
        println("wont happen!")
        last(t.left)
    else
        prevUp(t.up, t)
    end
end

function prevUp(t, from) 
    if t === nothing
        return nothing
    end
    if from == t.right
        if t.value !== nothing
            println("wont happen!")
            return t
        elseif t.left !== nothing
            return last(t.left)
        end
    end
    return prevUp(t.up, t)
end

function nextValue(t)
    if t.right !== nothing
        println("wont happen!")
        first(t.right)
    else
        nextUp(t.up, t)
    end
end

function nextUp(t, from) 
    if t === nothing
        return nothing
    end
    if from == t.left
        if t.value !== nothing
            println("wont happen!")
            return t
        elseif t.right !== nothing
            return first(t.right)
        end
    end
    return nextUp(t.up, t)
end

function explode(t)
    tp = prevValue(t.left)
    if tp !== nothing
        tp.value += t.left.value
    end

    tn = nextValue(t.right)
    if tn !== nothing
        tn.value += t.right.value
    end

    t.value = 0
    t.left = nothing
    t.right = nothing
end

function split(t)
    t.left = Tree2(floor(t.value/2), nothing, nothing, t)
    t.right = Tree2(ceil(t.value/2), nothing, nothing, t)
    t.value = nothing
    t
end

function checkExplode(t, level)
    if t === nothing
        return false
    end

    left_exploded = checkExplode(t.left, level+1)

    if !left_exploded && level > 4 && t.left !== nothing && t.left.value !== nothing
        explode(t)
        return true
    end

    left_exploded || checkExplode(t.right, level+1)
end

function checkSplit(t)
    if t === nothing
        return false
    end

    left_split = checkSplit(t.left)

    if !left_split && t.value !== nothing && t.value >= 10
        split(t)
        return true
    end

    left_split || checkSplit(t.right)
end

for s in [
    "[[[[[9,8],1],2],3],4]",
    "[7,[6,[5,[4,[3,2]]]]]",
    "[[6,[5,[4,[3,2]]]],1]",
    "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
]
    t = p2(s)
    # checkExplode(t, 1)
    # printTree(t)
end

function reduceNumber(t)
    # print("reducing: "); printTree(t)
    doit = true
    while doit
        doit = checkExplode(t, 1) || checkSplit(t)
        # print("result: "); printTree(t)
    end
end

function addNumbers(t_list)
    acc = t_list[1]
    for tree in t_list[2:end]
        # print("adding: "); printTree(tree)
        new_tree = Tree2(nothing, acc, tree, nothing)
        acc.up = new_tree
        tree.up = new_tree
        reduceNumber(new_tree)
        acc = new_tree

        # print("res: "); printTree(acc)
    end
    acc
end

function magnitude(t)
    if t.left !== nothing || t.right !== nothing
        return 3 * magnitude(t.left) + 2 * magnitude(t.right)
    else
        return t.value
    end
end

function part1()
    trees = [parseTree(line) for line in p]
    res = addNumbers(trees)
    printTree(res)
    magnitude(res)
end
part1()

## part 2


function part2()
    best = 0
    for t1 in 1:length(p)
        for t2 in 1:length(p)
            if t1 == t2
                continue
            end

            pt1 = parseTree(p[t1])
            pt2 = parseTree(p[t2])

            res = addNumbers([pt1, pt2])
            magn = magnitude(res)
            best = max(best, magn)
            println(magn)
        end
    end
    println(best)

    # 4562 too low
end
part2()