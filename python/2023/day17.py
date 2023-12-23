import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day17{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    print("arrived", find_path(lines, 0, 3))


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    print("arrived", find_path(lines, 4, 10))
    # 1213 too low


def find_path(lines, min_straight, max_straight):
    q = PriorityQueue()
    fake_start_cost = -int(lines[0][0])
    q.put((fake_start_cost, 0, 0, None, 0))
    visited = set()

    while not q.empty():
        cost, y, x, dir, repeat = q.get()

        if y < 0 or x < 0 or y >= len(lines) or x >= len(lines[y]):
            continue

        key = (y, x, dir, repeat)
        if key in visited:
            continue
        visited.add(key)

        cost += int(lines[y][x])

        if y == len(lines) - 1 and x == len(lines[y]) - 1 and repeat >= min_straight:
            return cost

        for new_dir in range(4):
            if dir is not None: 
                opposite = (dir + 2) % 4
                if new_dir == opposite:
                    continue
                if new_dir != dir and repeat < min_straight:
                    continue
                if new_dir == dir and repeat >= max_straight:
                    continue
            dd = dirs[new_dir]
            q.put((cost, y + dd[0], x + dd[1], new_dir, repeat + 1 if new_dir == dir else 1))


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
