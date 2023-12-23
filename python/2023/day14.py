import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day14{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


dirs = [(-1, 0), (0, -1), (1, 0), (0, 1)]


def problem1():
    with open(infile) as fp:
        lines = [[c for c in l.strip()] for l in fp]

    for l in lines:
        print("".join(l))
    print()

    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == "O":
                move_rock(lines, y, x, (-1, 0))

    for l in lines:
        print("".join(l))
    print()

    load = 0
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == "O":
                load += len(lines) - y
    print(load)


def move_rock(lines, y, x, dir):
    y2, x2 = y + dir[0], x + dir[1]
    if y2 < 0 or y2 >= len(lines) or x2 < 0 or x2 >= len(lines[0]):
        return

    if lines[y2][x2] == "O":
        move_rock(lines, y2, x2, dir)
    
    if lines[y2][x2] == ".":
        lines[y2][x2] = "O"
        lines[y][x] = "."
        move_rock(lines, y2, x2, dir)


def problem2():
    with open(infile) as fp:
        lines = [[c for c in l.strip()] for l in fp]

    iters = 1_000_000_000
    s = dict()
    i = 0
    while i < iters:
        for d in dirs:
            for y, l in enumerate(lines):
                for x, c in enumerate(l):
                    if c == "O":
                        move_rock(lines, y, x, d)
        
        key = "".join(["".join(l) for l in lines])
        if key not in s:
            s[key] = i
        else:
            repeat_len = i - s[key]
            repeat_times = (iters - i) // repeat_len
            if repeat_times > 0:
                print("found repeat", i, s[key], "length", repeat_len, repeat_times, "times")
                i += repeat_times * repeat_len
        i += 1

        load = 0
        for y, l in enumerate(lines):
            for x, c in enumerate(l):
                if c == "O":
                    load += len(lines) - y
        print(i, load)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
