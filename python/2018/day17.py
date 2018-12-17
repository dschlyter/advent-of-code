import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re
import sys

import util

from multiprocessing import Pool

filename = 'input/day17.txt'


def main():
    problem1()
    problem2()

# 73425 too high


@util.timing
def problem1():
    sys.setrecursionlimit(15000)

    lines = util.read_input(filename)
    clay, (ymin, ymax) = parse(lines)
    source = (500, ymin)

    water = set()
    dfs(clay, water, source, ymax)
    print(len(water))
    print(sorted(list(water)))


def dfs(clay, water, point, ymax):
    (x, y) = point
    if y > ymax:
        return False
    if point in water:
        return True
    if point in clay:
        return True

    water |= {point}

    d = dfs(clay, water, (x, y+1), ymax)
    if not d:
        return False
    l = dfs(clay, water, (x-1, y), ymax)
    r = dfs(clay, water, (x+1, y), ymax)
    return l and r


def parse(lines):
    clay = set()
    ymin, ymax = 999999999, 0

    for l in lines:
        [s, a, b] = util.parse_int_line(l)
        for i in range(a, b+1):
            if l.startswith("x"):
                clay |= {(s, i)}
                ymin = min(i, ymin)
                ymax = max(i, ymax)
            elif l.startswith("y"):
                clay |= {(i, s)}
                ymin = min(s, ymin)
                ymax = max(s, ymax)
            else:
                raise Exception("parse fail")
    return clay, (ymin, ymax)


@util.timing
def problem2():
    lines = util.read_input(filename)



if __name__ == '__main__':
    main()
