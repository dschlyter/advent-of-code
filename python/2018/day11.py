import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day10.txt'


def main():
    problem1()
    problem2()

input = 7165


@util.timing
def problem1():
    best = 0
    for i in range(1, 300 - 1):
        for j in range(1, 300 - 1):
            sum = 0
            for i2 in range(3):
                for j2 in range(3):
                    sum += power_level(i+i2, j+j2)
            if sum > best:
                best = sum
                print(i, j, sum)


def power_level(x, y):
    rackId = x + 10
    x = (rackId * y + input) * rackId
    return math.floor(x / 100) % 10 - 5


grid = []


@util.timing
def problem2():
    # precalc
    global grid
    grid.append([0]*301)
    for x in range(1, 301):
        grid.append([0]*301)
        sum = 0
        for y in range(1, 301):
            sum += power_level(x, y)
            grid[x][y] = sum + grid[x-1][y]

    pool = Pool()
    res = pool.map(solve2, range(1, 301))
    print(list(sorted(res))[-1])


def solve2(size):
    best = 0
    ret = []
    for i in range(1, 301 + 1 - size):
        for j in range(1, 301 + 1 - size):
            s = size-1
            sum = grid[i+s][j+s]
            sum -= grid[i+s][j-1]
            sum -= grid[i-1][j+s]
            # add this part that was subtracted twice
            sum += grid[i-1][j-1]
            if sum > best:
                best = sum
                ret = [sum, i, j, size]
    return ret


if __name__ == '__main__':
    main()
