function count_values(values)
    d = Dict()
    for v in values
        d[v] = get(d, v, 0) + 1
    end
    d
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

function cap(pos, low, high)
    max(low, min(pos, high))
end

# push to priority queue, overwrite only if lower
function prio_push(q, node, score)
    q[node] = min(score, get(q, node, score))
end