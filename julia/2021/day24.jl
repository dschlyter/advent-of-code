using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

input = readlines("input/day24.txt")
# input = readlines("input/day24-test2.txt")

input2 = [
    "inp w",
    "add z w",
    "mod z 2",
    "div w 2",
    "add y w",
    "mod y 2",
    "div w 2",
    "add x w",
    "mod x 2",
    "div w 2",
    "mod w 2"
]

p = [split(l) for l in input]

## part 1

function read_reg(reg, key)
    if key === nothing
        return key
    end
    if haskey(reg, key)
        return reg[key]
    end
    # return parse(BigInt, key)
    return parse(Int, key)
end

function run_program(program, input)
    input_read = 0
    default = 0
    reg = Dict(
        "w" => default,
        "x" => default,
        "y" => default,
        "z" => default,
    )

    xm, ym, zm, wm = 0,0,0,0

    for inst in program
        cmd = inst[1]
        a = inst[2]
        av = read_reg(reg, a)
        b = get(inst, 3, nothing)
        bv = read_reg(reg, b)

        if cmd == "inp"
            input_read += 1
            reg[a] = input[input_read]
        elseif cmd == "add"
            reg[a] = av + bv
        elseif cmd == "mul"
            reg[a] = av * bv
        elseif cmd == "div"
            if bv == 0
                return -1, input_read
            end
            reg[a] = trunc(av / bv)
        elseif cmd == "mod"
            if av < 0 || bv <= 0
                return -1, input_read
            end
            reg[a] = av % bv
        elseif cmd == "eql"
            if bv != 0
                log("eql", av, bv, av == bv, "(z =", reg["z"], ")")
            end
            reg[a] = av == bv ? 1 : 0
        else
            throw("Inst fail")
        end

        xm = max(reg["x"], xm)
        ym = max(reg["y"], ym)
        zm = max(reg["z"], zm)
        wm = max(reg["w"], wm)
    end

    log(xm, ym, zm, wm)
    return reg["z"], input_read
end

function decrease(input, index)
    if index == 1 && input[1] == 1
        throw("Cannot decrease more")
    end
    if input[index] > 1
        input[index] -= 1
    else
        for i in index:length(input)
            input[i] = 9
        end
        decrease(input, index-1)
    end
end

function part1_old()
    input = [9 for i in 1:14]
    # input = [parse(Int, c) for c in "99999998216338"]

    for i in 1:100000000
        res, input_read = run_program(p, input)

        break

        if res == 0
            log("success!", input_read)
            println(join([string(i) for i in input], ""))
            # break
        end
        decrease(input, input_read)

        if i % 10000 == 0
            println(input)
        end
    end
    println("done!")
    # println(join([string(i) for i in input], ""))

    # 99999998216338 too high - this was just after 1M iterations
end

# --- attempt 2

state_count = 0
best = 10^9
best_res = nothing
best_pos = 1

function run_program2(program, pos, reg1, seen, search)
    # Check end
    if pos > length(program)
        if best == 10^9
            log("the end?")
            best -= 1
        end
        global best
        if reg1["z"] < best
            best = reg1["z"]
            best_res = search
            log("found success!", search, best)
            if reg1["z"] == 0
                return search
            end
        end
        return nothing
    end

    global best_pos
    if pos > best_pos
        best_pos = pos
        log("pos", best_pos, length(search))
    end

    inst = program[pos]
    cmd = inst[1]
    a = inst[2]
    av = read_reg(reg1, a)
    b = get(inst, 3, nothing)
    bv = read_reg(reg1, b)

    global state_count
    state_count += 1
    if state_count % 1000000 == 0
        log(state_count, search)
    end

    # reg = deepcopy(reg1)
    reg = reg1

    if cmd == "inp"
        # in each step we divide by 26, so number has an upper bound before it cannot be reduced
        if reg["z"] > 26^min(8, 14 - length(search) + 2)
            return nothing
        end

        key = (pos, reg["z"])
        if key in seen
            return nothing
        end
        push!(seen, key)

        # for part 1 vs part 2
        # for input in 9:-1:1
        for input in 1:9
            reg = deepcopy(reg1)

            if pos == 1
                log("testing first", input)
            end
            reg[a] = input
            res = run_program2(program, pos+1, reg, seen, vcat(search, [input]))
            if res !== nothing
                return res
            end
        end
        return nothing
    else
        if cmd == "add"
            reg[a] = av + bv
        elseif cmd == "mul"
            reg[a] = av * bv
        elseif cmd == "div"
            if bv == 0
                return nothing
            end
            reg[a] = trunc(av / bv)
        elseif cmd == "mod"
            if av < 0 || bv <= 0
                return nothing
            end
            reg[a] = av % bv
        elseif cmd == "eql"
            reg[a] = av == bv ? 1 : 0
        else
            throw("Inst fail")
        end

        return run_program2(program, pos+1, reg, seen, search)
    end
end

function part1_2()
    seen = Set()
    # default = BigInt(0)
    default = 0
    reg = Dict(
        "w" => default,
        "x" => default,
        "y" => default,
        "z" => default,
    )

    res = run_program2(p, 1, reg, seen, [])
    println("res: ", res)
    if res !== nothing
        println(join([string(i) for i in res], ""))
    end

    # 99999998216338 too high - this was just state after 1M iterations
end
@time part1_2()

log("best result", best, best_res)

## attept 3 - partually manual

function part1_3()
    input = [9 for i in 1:14]
    # input = [parse(Int, c) for c in "99999998216338"]
    println(join([string(i) for i in input], ""))

    # input = [9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9]
    # input = [1, 9, 9, 9, 1, 7, 4, 6, 9, 9, 9, 9, 9, 4]
    # input = [1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9]
    input = [9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 7, 9, 9, 4]

    for i in 1:9
        println(input)

        res, input_read = run_program(p, input)

        log(res, input_read)
        break
    end
end
@time part1_3()


## part 2

function part2()

end
@time part2()
