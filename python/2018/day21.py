import math
import sys

import glob
import time
from datetime import datetime

import pydash

from string import ascii_lowercase
from queue import Queue
import re
import sys

import util

from multiprocessing import Pool

sys.setrecursionlimit(15000)
input_name = 'input/day21'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)
        # problem2_intepreted(file)


@util.timing
def problem1(filename):
    lines = util.read_input(filename)
    regs = [0] * 6
    ip_reg = util.parse_int_line(lines[0])[0]
    ops = []
    args = []
    for l in lines[1:]:
        ops.append(l.split(" ")[0])
        args.append(util.parse_int_line(l))

    regs[0] = 5745418

    for i in range(1000000):
        ip = regs[ip_reg]
        # solve 1, set breakpoint here
        if not (ip >= 0 and ip < len(ops)):
            break
        o = ops[ip]
        a = args[ip]
        operations[o](regs, a)
        regs[ip_reg] += 1

    print(i)


@util.timing
def problem2(filename):
    found = set()

    iterations = 0
    keep_looking = True

    r0, r1, r2, r3, r4, r5 = 0, 0, 0, 0, 0, 0

    # program rewritten in python and optimizec

    r4 = 123
    r4 = r4 & 456
    if not r4 == 72:
        raise Exception("inf loop")

    # 4
    # seti 0 0 5 # included above
    r4 = 0
    # 6
    # for i in range(100):
    # 100 values --> 6876956
    while keep_looking:
        iterations += 1
        r3 = r4 | 65536
        r4 = 14464005

        # 8 - start
        while True:
            r2 = r3 & 255
            r4 = r2 + r4
            r4 = r4 & 16777215
            r4 = r4 * 65899

            r4 = r4 & 16777215
            r4 = int_overflow(r4)
            if 256 > r3:
                if iterations == 100 and r4 != 6876956:
                    raise Exception("you effed something up when refactoring")
                # jump to 7
                # r5 = 27 # inlined below
                # if not r4 == r0:
                # r5 = 5
                if r4 not in found:
                    found |= {r4}
                    print(iterations, len(found), r4)
                else:
                    keep_looking = False
                break

            # 16
            # r5 = 27 # above
            r2 = 0
            # 18
            d = math.ceil((r3 + 1) / 256)
            r3 = d - 1
            continue

            # optimized away
            while True:
                r1 = r2 + 1
                r1 = r1 * 256

                if r1 > r3:
                    r3 = r2
                    break
                    # r5 = 7

                # 24
                r2 = r2 + 1
                # r5 = 17

    # 28
    # moved up


@util.timing
def problem2_intepreted(filename):
    lines = util.read_input(filename)
    regs = [0] * 6
    ip_reg = util.parse_int_line(lines[0])[0]
    ops = []
    args = []
    for l in lines[1:]:
        ops.append(l.split(" ")[0])
        args.append(util.parse_int_line(l))

    found_values = set()

    matches = 0
    while matches < 100:
        ip = regs[ip_reg]
        if ip == 28:
            matches += 1
            if regs[4] not in found_values:
                found_values |= {regs[4]}
                print(datetime.now(), regs[4])
        # make sure it does not terminate
        if ip > 30:
            ip = 5
        if not (ip >= 0 and ip < len(ops)):
            break
        o = ops[ip]
        a = args[ip]
        operations[o](regs, a)
        regs[ip_reg] += 1


def addr(registers, op):
    [a, b, c] = op
    registers[c] = int_overflow(registers[a] + registers[b])


def addi(registers, op):
    [a, b, c] = op
    registers[c] = int_overflow(registers[a] + b)


def mulr(registers, op):
    [a, b, c] = op
    registers[c] = int_overflow(registers[a] * registers[b])


def muli(registers, op):
    [a, b, c] = op
    registers[c] = int_overflow(registers[a] * b)


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


def int_overflow(val):
    if not -sys.maxsize-1 <= val <= sys.maxsize:
        val = (val + (sys.maxsize + 1)) % (2 * (sys.maxsize + 1)) - sys.maxsize - 1
    return val


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
