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

from pprint import pprint

sys.setrecursionlimit(15000)
input_name = 'input/day6'


def main():
    files = glob.glob(f"{input_name}*")
    for file in sorted(files, reverse=True):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    points = util.parse_ints(filename)

    fields = {}
    infinite = set()

    for p in points:
        fields[tuple(p)] = set()

    max_size = 400
    for x in range(max_size+1):
        for y in range(max_size+1):
            best_dist = sys.maxsize
            best_point = None

            for p in points:
                d = dist(p, (x, y))
                if d < best_dist:
                    best_dist = d
                    best_point = p
                elif d == best_dist:
                    best_point = None

            if best_point is not None:
                bp = tuple(best_point)
                if x == 0 or y == 0 or x == max_size or y == max_size:
                    infinite.add(bp)
                fields[bp].add((x, y))

    best_size = 0
    best_point = None
    for pl in points:
        p = tuple(pl)
        if p in infinite:
            continue
        size = len(fields.get(p, set()))
        if size > best_size:
            best_size = size
            best_point = p

    print(best_size, best_point)

    # 3010 is too low


def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


@util.timing
def problem2(filename):
    points = util.parse_ints(filename)

    max_dist = 10000 if len(points) > 30 else 32
    max_start_dist = max_dist // len(points)

    xmin = min([p[0] for p in points])
    xmax = max([p[0] for p in points])
    ymin = min([p[1] for p in points])
    ymax = max([p[1] for p in points])

    area_size = 0
    for x in range(xmin - max_start_dist, xmax + max_start_dist):
        for y in range(ymin - max_start_dist, ymax + max_start_dist):
            total = 0
            for p in points:
                total += dist(p, (x, y))
            if total < max_dist:
                area_size += 1

    print(area_size)




if __name__ == '__main__':
    main()
