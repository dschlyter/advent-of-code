import util
import copy
import math
import sys
import random
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day25{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    c = defaultdict(list)
    for l in lines:
        k, vs = l.split(": ")
        for v in vs.split(" "):
            c[k].append(v)
            c[v].append(k)

    ss = defaultdict(int)
    for k, v in c.items():
        ss[len(v)] += 1
    print(ss)

    for i in range(1000):
        conns, n1c, n2c = karger(c)
        if conns == 3:
            print(i, conns, (len(n1c) + 1) * (len(n2c) + 1))
            print("children", len(n1c), n1c)
            print("children", len(n2c), n2c)
            break


def karger(edges):
    # random.seed(4712)

    multi = defaultdict(lambda: defaultdict(int))
    for e, vs in edges.items():
        for v in vs:
            multi[e][v] += 1
    nodes = set(edges.keys())
    children = defaultdict(lambda: [])

    while len(nodes) > 2:
        n = random.choice(sorted(nodes))

        n_keys = sorted(multi[n].keys())
        n2 = random.choices(n_keys, weights=[multi[n][k] for k in n_keys], k=1)[0]
        if n == n2:
            raise Exception("sanity check: merge with same")

        nodes.remove(n2)
        children[n].append(n2)
        for v, conns in multi[n2].items():
            del multi[v][n2]
            if v != n:
                multi[v][n] += conns
                multi[n][v] += conns

    n1, n2 = sorted(nodes)
    n1c = expand_children(children, n1)
    n2c = expand_children(children, n2)
    if len(multi[n1]) != 1 or len(multi[n2]) != 1:
        raise Exception("sanity check: not a cut")
    print(multi[n1][n2], "conns")
    if len(n1c) + len(n2c) != len(edges) - 2:
        raise Exception("sanity check: missing children")
    return multi[n1][n2], n1c, n2c


def expand_children(c, n):
    r = set()
    r.update(c[n])
    for n2 in c[n]:
        r.update(expand_children(c, n2))
    return r



def problem2():
    pass


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
