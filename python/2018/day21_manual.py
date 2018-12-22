import datetime
import math
import sys

import glob
import pydash

from string import ascii_lowercase
from queue import Queue
import re
import sys

import util

from multiprocessing import Pool

sys.setrecursionlimit(15000)
input_name = 'input/day20'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)

# 73425 too high


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
def problem2(filename):
    lines = util.read_input(filename)

    found = set()

    r0, r1, r2, r3, r4, r5 = 0, 0, 0, 0, 0, 0

    r4 = 123
    r4 = r4 & 456
    if not r4 == 72:
        raise Exception("inf loop")

    # seti 0 0 5 # included above
    r4 = 0
    # 6
    while True:
        r3 = r4 | 65536
        r4 = 14464005

        # 8 - start
        while True:
            r2 = r3 & 255
            r4 = r2 + r4
            r4 = r4 & 16777215
            r4 = r4 * 65899

            r4 = r4 & 16777215
            if 256 > r2:
                # jump to 7
                # r5 = 27
                # if not r4 == r0:
                    # break
                    # r5 = 5
                if r4 not in found:
                    found |= {r4}
                print(datetime.now(), regs[4])

            # 16
            # r5 = 27 # above
            r2 = 0
            # 18
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

#ip 5
seti 123 0 4
bani 4 456 4
eqri 4 72 4
addr 4 5 5

seti 0 0 5
seti 0 8 4
bori 4 65536 3
seti 14464005 5 4

# 8
bani 3 255 2
addr 4 2 4
bani 4 16777215 4
muli 4 65899 4

# 12
bani 4 16777215 4
gtir 256 3 2
addr 2 5 5
addi 5 1 5

# 16
seti 27 7 5
seti 0 3 2
addi 2 1 1
muli 1 256 1

# 20
gtrr 1 3 1
addr 1 5 5
addi 5 1 5
seti 25 2 5

# 24
addi 2 1 2
seti 17 9 5
setr 2 2 3
seti 7 3 5

# 28
eqrr 4 0 2
addr 2 5 5
seti 5 9 5


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
