import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

filename = 'input/day10.txt'


def main():
    problem1()
    problem2()


def problem1():
    p = util.parse_ints(filename)
    s = 0
    for i in range(100000):
        s += 1
        apply_velocity(p)
        b = bounds(p)
        [x1, x2, y1, y2] = b
        if x2 - x1 < 100 and y2 - y1 < 100:
            print(s)
            show(p, b)


def apply_velocity(points):
    for p in points:
        p[0] += p[2]
        p[1] += p[3]


def bounds(points):
    m = sys.maxsize
    x1, x2, y1, y2 = m, -m, m, -m
    for p in points:
        x1 = min(x1, p[0])
        x2 = max(x2, p[0])
        y1 = min(y1, p[1])
        y2 = max(y2, p[1])
    return [x1, x2, y1, y2]


def show(points, bounds):
    [x1, x2, y1, y2] = bounds
    canvas = []
    for i in range(y2 - y1 + 2):
        canvas.append([" "] * (x2 - x1 + 1))

    for p in points:
        x = p[0] - x1
        y = p[1] - y1
        canvas[y][x] = "#"

    canvas = list(map(lambda l: "".join(l), canvas))
    for line in canvas:
        print(line)
    print("-----")


def problem2():
    pass


if __name__ == '__main__':
    main()
