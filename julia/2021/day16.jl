using Printf
using Pkg

# Pkg.add("DataStructures"); 
# using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

function parseInput2(line)
    [parse(Int, p) for p in line]
end

input = readlines("input/day16.txt")
# input = readlines("input/day16-test.txt")

function parse_string(inp)
    r = []
    for s in inp
        p = parse(Int, s, base=16)
        b = lpad(string(p, base=2), 4, "0")
        append!(r, b)
    end
    string(r...)
end

bits = parse_string(input[1])

## part 1

offset = 1

function read_packet(bstr, i)
    v = parse(Int, bstr[i:i+2], base=2)
    i += 3
    t = parse(Int, bstr[i:i+2], base=2)
    i += 3

    println("packet ", v, " ", t)

    if t == 4
        num = 0
        while true
            s = bstr[i:i+4]
            i += 5
            num = num * 16 + parse(Int, s[2:5], base=2)
            if s[1] == '0'
                break
            end
        end
        return i, (v, t, num)
    else
        mode = parse(Int, bstr[i:i], base=2)
        i += 1
        sub_packets = []
        if mode == 0
            len = parse(Int, bstr[i:i+14], base=2)
            i += 15
            limit = i + len
            while i < limit
                i, sp = read_packet(bstr, i)
                push!(sub_packets, sp)
                if i > limit
                    println("Warning! Exceeded limit! ", i, " ", limit)
                end
            end
        elseif mode == 1
            packets = parse(Int, bstr[i:i+10], base=2)
            i += 11
            for p in 1:packets
                i, sp = read_packet(bstr, i)
                push!(sub_packets, sp)
            end
        end

        return i, (v, t, sub_packets)
    end
end

function version_sum(s)
    v = s[1]
    if s[2] != 4
        for sp in s[3]
            v += version_sum(sp)
        end
    end
    v
end

function part1()
    i, p = read_packet("00111000000000000110111101000101001010010001001000000000", 1)
    println(p)

    i, p = read_packet("11101110000000001101010000001100100000100011000001100000", 1)
    println(p)

    for hex in [
        "8A004A801A8002F478",
        "620080001611562C8802118E34",
        "C0015000016115A2E0802F182340",
        "A0016C880162017C3686B18A3D4780",
        input[1]
    ]
        println(hex)
        b = parse_string(hex)
        println(b)
        i, p = read_packet(b, 1)
        println(p)
        println(version_sum(p))
    end
end
part1()

## part 2

function parse_expr(e)
    t = e[2]
    if t == 4
        return e[3]
    end

    sr = [parse_expr(p) for p in e[3]]
    if t == 0
        return sum(sr)
    elseif t == 1
        return reduce(*, sr)
    elseif t == 2
        return min(sr...)
    elseif t == 3
        return max(sr...)
    elseif t == 5
        return sr[1] > sr[2] ? 1 : 0
    elseif t == 6
        return sr[1] < sr[2] ? 1 : 0
    elseif t == 7
        return sr[1] == sr[2] ? 1 : 0
    else
        println("Missing! ", t)
        return -1
    end
end

function part2()
    for hex in [
        "C200B40A82",
        "04005AC33890",
        "880086C3E88112",
        "CE00C43D881120",
        "D8005AC2A8F0",
        "F600BC2D8F",
        "9C005AC2F8F0",
        "9C0141080250320F1802104A08",
        input[1]
    ]
        println(hex)
        b = parse_string(hex)
        i, p = read_packet(b, 1)
        res = parse_expr(p)
        println(res)
    end
end
part2()