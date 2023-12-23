import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day16{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]
    print(energized_by(lines, (0, 0, 0, 1)))


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    starts = []
    for y in range(len(lines)):
        starts.append((y, 0, 0, 1))
        starts.append((y, len(lines[0]) - 1, 0, -1))
    for x in range(len(lines[0])):
        starts.append((0, x, 1, 0))
        starts.append((len(lines) - 1, x, -1, 0))
    ans = 0
    for s in starts:
        r = energized_by(lines, s)
        ans = max(ans, r)
    print("best", ans)


def energized_by(lines, beam):
    q = Queue()
    q.put(beam)
    visited = set()

    while not q.empty():
        k = q.get()
        y, x, yd, xd = k

        if y < 0 or y >= len(lines) or x < 0 or x >= len(lines[y]):
            continue

        if k in visited:
            continue
        visited.add(k)

        t = lines[y][x]
        if t == "." or (t == "-" and yd == 0) or (t == "|" and xd == 0):
            q.put((y + yd, x + xd, yd, xd))
        elif t == "-":
            q.put((y, x + 1, 0, 1))
            q.put((y, x - 1, 0, -1))
        elif t == "|":
            q.put((y + 1, x, 1, 0))
            q.put((y - 1, x, -1, 0))
        elif t == "/":
            nyd = -xd
            nxd = -yd
            q.put((y + nyd, x + nxd, nyd, nxd))
        elif t == "\\":
            nyd = xd
            nxd = yd
            q.put((y + nyd, x + nxd, nyd, nxd))

    visited_tiles = {(y, x) for (y, x, _, _) in visited}
    return len(visited_tiles)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
