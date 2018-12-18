import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re
import sys

import util

from multiprocessing import Pool

filename = 'input/day18.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    lines = util.read_input(filename)
    #for line in lines:
    #    print(line)

    grid = lines
    for i in range(10):
        grid = transform(grid)

    print("---")
    for line in grid:
        print("".join(line))

    lumber, yard = 0, 0
    for y in range(0, len(grid)):
        for x in range(0, len(grid[y])):
            c = grid[y][x]
            if c == '|':
                lumber += 1
            if c == '#':
                yard += 1
    print(lumber * yard)


def transform(grid):
    new_grid = []
    for y in range(0, len(grid)):
        line = grid[y]
        new_grid.append([])
        for x in range(0, len(line)):
            c = line[x]
            if c == '.' and count_nearby(grid, y, x, '|') >= 3:
                c = '|'
            elif c == '|' and count_nearby(grid, y, x, '#') >= 3:
                c = '#'
            elif c == '#' and not (count_nearby(grid, y, x, '#') >= 1 and count_nearby(grid, y, x, '|') >= 1):
                c = '.'
            new_grid[-1].append(c)
    return new_grid


def count_nearby(grid, y, x, target):
    count = 0
    for yn in range(y-1, y+2):
        for xn in range(x-1, x+2):
            if xn == x and yn == y:
                continue
            if yn < 0 or yn >= len(grid):
                continue
            line = grid[yn]
            if xn < 0 or xn >= len(line):
                continue
            if line[xn] == target:
                count += 1
    return count


@util.timing
def problem2():
    lines = util.read_input(filename)

    grid = lines
    target = 1000000000
    i = 0
    # a horrible hack
    cycles = {}
    value = -1
    while i < target:
        i += 1

        if i > 10000:
            value = cycles[value]
            continue

        grid = transform(grid)

        if i % 10000 == 0:
            print("progress", i/target)

        # print("---")
        # for line in grid:
            # print("".join(line))

        lumber, yard = 0, 0
        for y in range(0, len(grid)):
            for x in range(0, len(grid[y])):
                c = grid[y][x]
                if c == '|':
                    lumber += 1
                if c == '#':
                    yard += 1
        new_value = lumber * yard
        print(i, value, new_value, cycles.get(value) is None)
        cycles[value] = new_value
        value = new_value

    print(value)


if __name__ == '__main__':
    main()
