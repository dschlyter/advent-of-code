import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

suffix = sys.argv[1] if len(sys.argv) > 1 else ''
infile = f'input/day11{suffix}.txt'

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


dirs = [
    (-1, 0),
    (1, 0),
    (0, 1),
    (0, -1),
]


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    # add empty lines
    i = 0
    while i < len(lines):
        l = lines[i]
        if set(l) == {'.'}:
            lines.insert(i, copy.deepcopy(l))
            i += 1
        i += 1

    i = 0
    while i < len(lines[0]):
        if set([l[i] for l in lines]) == {'.'}:
            for j, l in enumerate(lines):
                lines[j] = l[:i] + '.' + l[i:]
            i += 1
        i += 1

    # calculate distance to all
    ans = 0
    for sy in range(len(lines)):
        for sx in range(len(lines[sy])):
            if lines[sy][sx] != '#':
                continue
            for y in range(len(lines)):
                for x in range(len(lines[y])):
                    if (sy, sx) <= (y, x):
                        continue
                    if lines[y][x] == '#':
                        dist = abs(sy - y) + abs(sx - x)
                        ans += dist
    print(ans)


def problem2():
    with open(infile) as fp:
        lines = [[c for c in l.strip()] for l in fp]

    # add empty lines
    i = 0
    while i < len(lines):
        l = lines[i]
        if "#" not in l:
            lines[i] = ["M"] * len(lines[i])
        i += 1

    i = 0
    while i < len(lines[0]):
        if "#" not in [l[i] for l in lines]:
            for j, l in enumerate(lines):
                lines[j][i] = "M"
        i += 1

    # calculate distance to all
    expansion = 1_000_000
    ans = 0
    for sy in range(len(lines)):
        for sx in range(len(lines[sy])):
            if lines[sy][sx] != '#':
                continue
            for y in range(len(lines)):
                for x in range(len(lines[y])):
                    if (sy, sx) <= (y, x):
                        continue
                    if lines[y][x] == '#':
                        # dist = abs(sy - y) + abs(sx - x)
                        dist = 0
                        for y2 in range(min(sy, y), max(sy, y)):
                            if lines[y2][sx] == 'M':
                                dist += expansion
                            else:
                                dist += 1
                        for x2 in range(min(sx, x), max(sx, x)):
                            if lines[y][x2] == 'M':
                                dist += expansion
                            else:
                                dist += 1
                        ans += dist
    print(ans)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
