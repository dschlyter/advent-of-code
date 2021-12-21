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

function log(args...)
    println(join([string(a) for a in args], " ")...)
end

# wrap position to range, inclusive-inclusive
function wrap(pos, low, high)
    # inefficient opt
    while pos < low
        pos += (high - low + 1)
    end
    (pos - low) % (high - low + 1) + low
end
