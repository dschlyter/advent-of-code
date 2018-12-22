import math
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
input_name = 'input/day22'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    lines = util.parse_ints(filename)
    depth = lines[0][0]
    [tx, ty] = lines[1]

    cave = Cave(depth, (tx, ty))
    s = 0

    for x in range(tx + 1):
        for y in range(ty + 1):
            s += cave.risk_level((x, y))

    for y in range(16):
        for x in range(16):
            e = cave.risk_level((x, y))
            if e == 0:
                print(".", end="")
            if e == 1:
                print("=", end="")
            if e == 2:
                print("|", end="")
        print()

    # 10352 too low

    print(s)


class Cave:
    def __init__(self, depth, target):
        self.depth = depth
        self.target = target
        self.mem = {}

    def erosion(self, pos):
        if self.mem.get(pos):
            return self.mem[pos]

        x, y = pos
        e = (self.geo(x, y) + self.depth) % 20183
        self.mem[pos] = e
        return e

    def geo(self, x, y):
        if x == 0 and y == 0:
            return 0
        if (x, y) == self.target:
            return 0
        if y == 0:
            return x * 16807
        if x == 0:
            return y * 48271
        return self.erosion((x-1, y)) * self.erosion((x, y-1))

    def region_type(self, pos):
        return self.erosion(pos) % 3

    def risk_level(self, pos):
        return self.region_type(pos)


@util.timing
def problem2(filename):
    lines = util.parse_ints(filename)
    depth = lines[0][0]
    [tx, ty] = lines[1]

    cave = Cave(depth, (tx, ty))
    t = search(cave, (tx, ty), torch)
    print(t)


rocky = 0
wet = 1
narrow = 2

neither = 0
torch = 1
climbing = 2


def search(cave, target_pos, target_tool):
    visited = set()
    q = PriorityQueue()
    q.put((0, (0, 0), torch))

    while not q.empty():
        (time, pos, tool) = q.get()
        if (pos, tool) in visited:
            continue
        visited.add((pos, tool))

        x, y = pos
        if x < 0 or y < 0:
            continue

        region = cave.region_type(pos)
        if not valid_tool(tool, region):
            continue

        if pos == target_pos and tool == target_tool:
            return time

        for new_tool in [neither, torch, climbing]:
            if new_tool != tool:
                q.put((time+7, pos, new_tool))

        for new_pos in nearby(pos):
            q.put((time+1, new_pos, tool))


def valid_tool(tool, region):
    return tool != region


def nearby(pos):
    (x, y) = pos
    return [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]


if __name__ == '__main__':
    main()
