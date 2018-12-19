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
    #ip 4 -> r4
    r0, r1, r2, r3, r4, r5 = 0, 0, 0, 0, 0, 0

    r4 = r4 + 16
    r2 = 1
    r5 = 1
    r3 = r2 * r5

    r3 = r1 == r3
    r4 = r3 + r4
    r4 = r4 + 1
    r0 = r2 + r0

    r5 = r5 + r1
    r3 = r5 > r1
    r4 = r3 + r4
    # restart-ish
    r4 = 2

    r2 = r2 + 1
    r3 = r2 > r1
    r1 = r1 * r1
    r4 = 1

    r4 = r4 * r4
    r1 = r1 + 2
    r1 = r1 + 2
    r1 = r4 * r1

    r1 = r1 * 11
    r3 = r3 * 3
    r3 = r3 * r4
    r3 = r3 + 9

    r1 = r1 + r3
    r4 = r4 + r0
    # restart
    r4 = 0
    r3 = r4

    r3 = r3 * r4
    r3 = r4 + r3
    r3 = r4 * r3
    r3 = r3 * 14

    r3 = r3 * r4
    r1 = r1 + r3
    r0 = 0
    # restart
    r4 = 0



    addi 4 16 4
    seti 1 7 2
    seti 1 1 5
    mulr 2 5 3

    eqrr 3 1 3
    addr 3 4 4
    addi 4 1 4
    addr 2 0 0

    addi 5 1 5
    gtrr 5 1 3
    addr 4 3 4
    seti 2 7 4

    addi 2 1 2
    gtrr 2 1 3
    addr 3 4 4
    seti 1 3 4

    mulr 4 4 4
    addi 1 2 1
    mulr 1 1 1
    mulr 4 1 1

    muli 1 11 1
    addi 3 3 3
    mulr 3 4 3
    addi 3 9 3

    addr 1 3 1
    addr 4 0 4
    seti 0 1 4
    setr 4 9 3

    mulr 3 4 3
    addr 4 3 3
    mulr 4 3 3
    muli 3 14 3

    mulr 3 4 3
    addr 1 3 1
    seti 0 6 0
    seti 0 7 4



@util.timing
def problem2():
    lines = util.read_input(filename)


if __name__ == '__main__':
    main()
