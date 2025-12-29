import queue
import util
import sys
from collections import defaultdict 
from functools import reduce
from operator import add, mul

from util import get

sys.setrecursionlimit(1_000_000)

def parse(lines):
    presents = []
    areas = []
    i = 0

    i = 0
    while i < len(lines):
        l = lines[i]
        if l == "":
            continue
        elif l.startswith(f"{len(presents)}:"):
            i += 1
            pres = []
            while lines[i] != "":
                pres.append(lines[i])
                i += 1
            presents.append(pres)
        elif "x" in l:
            s, p = l.split(":")
            a, b = s.split("x")
            areas.append(((int(a), int(b)), list(map(int, p.strip().split(" ")))))
        else:
            raise Exception("Unknown line in input: " + l)
        i += 1

    return presents, areas


def p1(lines):
    ans = 0

    presents, areas = parse(lines)

    actually_trivial = 0
    trivial = 0
    maybe = 0
    nope = 0

    for a in areas:
        dim, pres_counts = a
        size = dim[0] * dim[1]
        floored_size = (dim[0] - dim[0] % 3) * (dim[1] - dim[1] % 3)
        pres_sum = sum(pres_counts)
        present_blocks = sum(len([c for l in p for c in l if c == "#"]) * pres_counts[i] for i, p in enumerate(presents))

        if floored_size >= pres_sum * 9:
            actually_trivial += 1
        elif size >= pres_sum * 9:
            trivial += 1
        elif size <= present_blocks:
            nope += 1
        else:
            maybe += 1

    # lol - stupid basic size analysis just works...
    # there are zero in the "trivial" or "maybe" groups - all are actually trivial

    print(actually_trivial, trivial, maybe, nope)
    print("Part 1:", ans)

def p2(lines):
    ans = 0

    print("Part 2:", ans)

def main():
    TEST = "input/day12_test.txt"
    MAIN = "input/day12.txt"

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