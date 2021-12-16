function read_input()
    println("a func!")
end

function count_values(values)
    d = Dict()
    for v in values
        d[v] = get(d, v, 0) + 1
    end
    d
end

function dict_add()
end

function mode(values)
    d = count_values(values)
    pairs = collect(d)
    s = sort(pairs, by=x->x[2], rev=true)
    s[1][1]
end