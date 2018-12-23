import math
import random
import sys

import glob
import time
from datetime import datetime

import pydash

from string import ascii_lowercase
from queue import Queue, PriorityQueue
import re
import sys

import util

from multiprocessing import Pool

sys.setrecursionlimit(15000)
input_name = 'input/day23_test'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    lines = util.parse_ints(filename)

    best_range = 0
    best_bot = 0

    for l in lines:
        [x, y, z, r] = l
        if r > best_range:
            best_range = r
            best_bot = l

    count = 0
    for bot in lines:
        if dist(bot, best_bot) <= best_range:
            count += 1

    print(count)


def dist(bot, other_bot):
    [x, y, z, r] = bot
    [xb, yb, zb, rb] = other_bot
    return abs(xb - x) + abs(yb - y) + abs(zb - z)


@util.timing
def problem2(filename):
    lines = util.parse_ints(filename)

    best_range = 0
    best_bot = 0

    for l in lines:
        [x, y, z, r] = l
        if r > best_range:
            best_range = r
            best_bot = l

    points = []
    for i in range(10000):
        [x, y, z, r] = best_bot
        r1, r2, r3, r4 = random.random(), random.random(), random.random(), random.random()
        s = sum([r1, r2, r3, r4])
        r1 = r1 / s
        r2 = r2 / s
        r3 = r3 / s
        # the distance that is unused
        r4 = r4 / s
        xr = math.floor(x + sign() * r1 * r)
        yr = math.floor(y + sign() * r2 * r)
        zr = math.floor(z + sign() * r3 * r)
        points.append([xr, yr, zr, -1])

    best_count = 0
    best_point = 0
    for p in points:
        count = range_count(lines, p)
        if count > best_count:
            best_point = p
            best_count = count
    print(best_count)

    while True:
        stepped = False

        [x, y, z, r] = best_point
        nx = [smaller(x), y, z, r]
        if nx != best_point and range_count(lines, nx) >= best_count:
            best_point = nx
            stepped = True

        [x, y, z, r] = best_point
        ny = [x, smaller(y), z, r]
        if ny != best_point and range_count(lines, ny) >= best_count:
            best_point = ny
            stepped = True

        [x, y, z, r] = best_point
        nz = [x, y, smaller(z), r]
        if nz != best_point and range_count(lines, nz) >= best_count:
            best_point = nz
            stepped = True

        if not stepped:
            break

    print(dist(best_point, [0, 0, 0, 0]), best_point, best_count)


def in_range(point, bot):
    return dist(bot, point) <= bot[3]


def range_count(bots, point):
    count = 0
    for bot in bots:
        if in_range(point, bot):
            count += 1
    return count


def sign():
    return random.choice([-1, 1])


def smaller(x):
    if x > 0:
        return x-1
    elif x < 0:
        return x+1
    else:
        return 0


if __name__ == '__main__':
    main()
