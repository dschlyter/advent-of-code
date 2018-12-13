import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day12.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    pad = 40
    lines = util.read_input(filename)
    initial, trans = parse(lines)
    state = '.' * pad + initial + '.' * pad
    for gen in range(20):
        print(state)
        newState = ".."
        for i in range(2, len(state) - 2):
            newState += trans.get(state[i-2:i+3], '.')
        newState += ".."
        state = newState
    print(state)

    sum = 0
    for i in range(len(state)):
        if state[i] == "#":
            sum += (i-pad)
    print(sum)


def parse(lines):
    initial = lines[0].split(" ")[2]

    trans = {'.....': '.'}
    for l in lines[2:]:
        s = l.split(" ")
        trans[s[0]] = s[2]

    return initial, trans


@util.timing
def problem2():
    pad = 1020
    lines = util.read_input(filename)
    initial, trans = parse(lines)
    state = '.' * pad + initial + '.' * pad
    for gen in range(1000):
        print(state)
        newState = ".."
        for i in range(2, len(state) - 2):
            newState += trans.get(state[i-2:i+3], '.')
        newState += ".."
        state = newState
    print(state)

    sum = 0
    count = 0
    for i in range(len(state)):
        if state[i] == "#":
            count += 1
            sum += (i-pad)
    print(sum, count)
    print((50000000000 - 1000)*count + sum)


if __name__ == '__main__':
    main()
