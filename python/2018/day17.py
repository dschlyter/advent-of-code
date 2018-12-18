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


@util.timing
def problem1():
    sys.setrecursionlimit(15000)

    lines = util.read_input(filename)
    clay, (ymin, ymax) = parse(lines)
    source = (500, ymin)

    water = set()
    drained = set()
    dfs(clay, water, drained, source, ymax)

    util.print_states(clay, "#", water, ".", drained, "_", flip=True)
    print(len(water))
    print(len(water) - len(drained))


def dfs(clay, water, drained, point, ymax):
    (x, y) = point
    if y > ymax or point in drained:
        return True
    if point in clay or point in water:
        return False

    water |= {point}

    d = dfs(clay, water, drained, (x, y+1), ymax)
    if d:
        drain_plane(water, drained, point)
        return True
    l = dfs(clay, water, drained, (x-1, y), ymax)
    r = dfs(clay, water, drained, (x+1, y), ymax)
    if l or r:
        drain_plane(water, drained, point)
        return True

    return False


def drain_plane(water, drained, point):
    if point in water and point not in drained:
        drained |= {point}
        (x, y) = point
        drain_plane(water, drained, (x+1, y))
        drain_plane(water, drained, (x-1, y))


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
