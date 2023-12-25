import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day24{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    b, e = 200000000000000, 400000000000000
    if "test" in suffix:
        b, e = 7, 27

    hails = parse(lines)
    for i, hail in enumerate(hails):
        for j, hail2 in enumerate(hails):
            if i >= j:
                continue
            x1, y1, z1, xv1, yv1, zv1 = hail
            x2, y2, z2, xv2, yv2, zv2 = hail2

            k1 = yv1 / xv1
            m1 = y1 - x1 * k1
            k2 = yv2 / xv2
            m2 = y2 - x2 * k2
            if (k1 - k2) == 0:
                print(i, j, "paralell")
                continue
            x_cross = (m2 - m1) / (k1 - k2)
            y_cross = k1 * x_cross + m1
            t1 = (x_cross - x1) / xv1
            t2 = (x_cross - x2) / xv2

            valid = True
            if t1 < 0 or t2 < 0:
                valid = False
                print("in the past: ", end="")
            if not (b < x_cross < e and b < y_cross < e):
                valid = False
                print("outside: ", end="")
            print(i, j, x_cross, y_cross)
            if valid:
                ans += 1

    print(ans)


def parse(lines):
    r = []
    for l in lines:
        ps, vs = l.split("@")
        r.append(list(map(int, ps.strip().split(", "))) + list(map(int, vs.strip().split(", "))))
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
