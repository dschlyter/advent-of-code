import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day19.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    lines = util.read_input(filename)
    regs = [0] * 6
    ip_reg = util.parse_int_line(lines[0])[0]
    ops = []
    args = []
    for l in lines[1:]:
        ops.append(l.split(" ")[0])
        args.append(util.parse_int_line(l))

    while True:
        ip = regs[ip_reg]
        if not (ip >= 0 and ip < len(ops)):
            break
        o = ops[ip]
        a = args[ip]
        operations[o](regs, a)
        regs[ip_reg] += 1
        # print(regs[ip_reg], regs)

    print(regs, regs[0])


@util.timing
def problem2():
    lines = util.read_input(filename)

    # hand optimized
    r0, r1, r2, r3, r4, r5 = 1, 0, 0, 0, 0, 0

    r4 = r4 + 16
    r2 = 1
    r5 = 1
    # r3 = r2 * r5

    # r3 = r1 == r3
    # r4 = r3 + r4 # jump over one
    # r4 = r4 + 1 # jump over one
    # main loop, finds all prime ractors of r1, including 1 and r1 itself
    if r1 == r2 * r5:
        r0 = r2 + r0

    # 2
    r5 = r5 + 1
    # r3 = r5 > r1 # jump over one
    # r4 = r3 + r4
    if not r5 > r1:
        r4 = 2 # hoppas till 3

    r2 = r2 + 1
    # r3 = r2 > r1
    # r4 = r3 + r4
    if not r2 > r1:
        r4 = 1 # hoppa till 2

    # 4
    r4 = r4 * r4 # 16 * 16, instant terminate?
    # program start
    r1 = r1 + 2 # 2
    r1 = r1 * r1 # 4
    r1 = 19 * r1 # 76

    # 5
    r1 = r1 * 11 # 836
    r3 = r3 + 3 # 3
    r3 = r3 * r4 # 66
    r3 = r3 + 9 # 75

    # 6
    r1 = r1 + r3 # 911
    # r4 = r4 + r0
    if r0 == 0:
        r4 = 0 # hoppar till 1, hoppas över i problem2
    r3 = r4 # 27

    # 7
    r3 = r3 * r4 # 27*28 = 756
    r3 = r4 + r3 # 29+756 = 785
    r3 = r4 * r3 # 30 * 785 = 23550
    r3 = r3 * 14 # 329700

    # 8
    r3 = r3 * r4 # 10550400
    r1 = r1 + r3 # 10551311
    r0 = 0
    # restart
    r4 = 0 # hoppar till 1


def addr(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] + registers[b]


def addi(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] + b


def mulr(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] * registers[b]


def muli(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] * b


def banr(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] & registers[b]


def bani(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] & b


def borr(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] | registers[b]


def bori(registers, op):
    [a, b, c] = op
    registers[c] = registers[a] | b


def setr(registers, op):
    [a, b, c] = op
    registers[c] = registers[a]


def seti(registers, op):
    [a, b, c] = op
    registers[c] = a


def gtir(registers, op):
    [a, b, c] = op
    registers[c] = 1 if a > registers[b] else 0


def gtri(registers, op):
    [a, b, c] = op
    registers[c] = 1 if registers[a] > b else 0


def gtrr(registers, op):
    [a, b, c] = op
    registers[c] = 1 if registers[a] > registers[b] else 0


def eqir(registers, op):
    [a, b, c] = op
    registers[c] = 1 if a == registers[b] else 0


def eqri(registers, op):
    [a, b, c] = op
    registers[c] = 1 if registers[a] == b else 0


def eqrr(registers, op):
    [a, b, c] = op
    registers[c] = 1 if registers[a] == registers[b] else 0


operations = {
    'addr': addr,
    'addi': addi,
    'mulr': mulr,
    'muli': muli,
    'banr': banr,
    'bani': bani,
    'borr': borr,
    'bori': bori,
    'setr': setr,
    'seti': seti,
    'gtir': gtir,
    'gtri': gtri,
    'gtrr': gtrr,
    'eqir': eqir,
    'eqri': eqri,
    'eqrr': eqrr,
}

if __name__ == '__main__':
    main()
