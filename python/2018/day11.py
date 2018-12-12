import math
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


@util.timing
def problem2():
    # precalc
    grid = []
    for x in range(0, 301):
        grid.append([0]*301)
        sum = 0
        for y in range(1, 301):
            sum += power_level(x, y)
            grid[x][y] = sum

    best = 0
    # did not even need to finish all the way
    for size in range(1, 301):
        print("size", size)
        for i in range(1, 301 + 1 - size):
            for j in range(1, 301 + 1 - size):
                sum = 0
                for x in range(i, i+size):
                    y1, y2 = j, j+size-1
                    sum += grid[x][y2] - grid[x][y1-1]
                if sum > best:
                    best = sum
                    print(i, j, size, sum)


if __name__ == '__main__':
    main()
