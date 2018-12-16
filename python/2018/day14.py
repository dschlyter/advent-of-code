import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day14.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    lines = util.read_input(filename)
    stop = int(lines[0])
    # stop = 2018
    scores = [3, 7]

    elf1 = 0
    elf2 = 1
    while len(scores) < stop + 10:
        s = scores[elf1] + scores[elf2]
        for d in str(s):
            scores.append(int(d))
        elf1 = (elf1 + scores[elf1] + 1) % len(scores)
        elf2 = (elf2 + scores[elf2] + 1) % len(scores)

    for s in scores[stop:stop+10]:
        print(s, end="")
    print()


@util.timing
def problem2():
    lines = util.read_input(filename)
    stop = lines[0]
    # stop = "51589"
    scores = [3, 7]

    target = []
    for s in stop:
        target.append(int(s))
    target.reverse()

    elf1 = 0
    elf2 = 1
    m = len(stop)
    while True:
        s = scores[elf1] + scores[elf2]
        for d in str(s):
            scores.append(int(d))
        elf1 = (elf1 + scores[elf1] + 1) % len(scores)
        elf2 = (elf2 + scores[elf2] + 1) % len(scores)

        if match(scores, len(scores) - 1, target):
            break
        if match(scores, len(scores) - 2, target):
            m += 1
            break

    for s in scores[-10:]:
        print(s, end="")
    print()
    print(len(scores) - m)


def match(scores, i, target):
    j = 0
    while j < len(target):
        if scores[i] != target[j]:
            return False
        i -= 1
        j += 1
    return True


if __name__ == '__main__':
    main()
