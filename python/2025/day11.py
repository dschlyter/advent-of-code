import queue
import util
import sys
from collections import defaultdict 
from functools import reduce
from operator import add, mul

from util import get

sys.setrecursionlimit(1_000_000)

def parse(lines):
    ret = dict()
    for l in lines:
        k, vals = l.split(":")
        ret[k] = vals.strip().split(" ")
    return ret

def p1(lines):
    ans = 0

    conns = parse(lines)
    ans = dfs("you", conns, dict())

    print("Part 1:", ans)

def dfs(node, conns, mem):
    if node in mem:
        return mem[node]

    if node == "out":
        return 1

    if node not in conns or len(conns[node]) == 0:
        return 0

    total = 0
    for nei in conns[node]:
        total += dfs(nei, conns, mem)

    mem[node] = total
    return total

def p2(lines):
    ans = 0

    conns = parse(lines)
    ans = dfs2((0, 0, "svr"), conns, dict())

    print("Part 2:", ans)

def dfs2(state, conns, mem, l=[]):
    if state in mem:
        return mem[state]
    dac, fft, node = state

    if node == "out" and dac == 1 and fft == 1:
        return 1

    if node not in conns or len(conns[node]) == 0:
        return 0

    if node == "dac":
        dac = 1
    elif node == "fft":
        fft = 1

    total = 0
    for nei in conns[node]:
        total += dfs2((dac, fft, nei), conns, mem, l + [state])

    mem[state] = total
    return total

def main():
    TEST = "input/day11_test.txt"
    MAIN = "input/day11.txt"

    if len(sys.argv) > 1:
        if sys.argv[1] == "1":
            run(TEST)
        elif sys.argv[1] == "2":
            run(MAIN)
        else:
            run(sys.argv[1])
    else:
        run(TEST)
        run(MAIN)

def run(in_file):
    print("=== Running", in_file, "===")
    lines = util.read_input(in_file)
    p1(lines)
    p2(lines)

if __name__ == "__main__":
    main()