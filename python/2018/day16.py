import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day16.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    lines = util.read_input(filename)

    count = 0
    i = 0
    while i < len(lines) and lines[i]:
        before = util.parse_int_line(lines[i])
        op = util.parse_int_line(lines[i + 1])
        after = util.parse_int_line(lines[i + 2])

        m = test_op(before, op, after)
        if len(m) >= 3:
            count += 1

        i += 4

    print(count)


@util.timing
def problem2():
    lines = util.read_input(filename)

    possible_ops = {}

    i = 0
    while i < len(lines) and lines[i]:
        before = util.parse_int_line(lines[i])
        op = util.parse_int_line(lines[i + 1])
        after = util.parse_int_line(lines[i + 2])
        i += 4

        m = test_op(before, op, after)
        opcode = op[0]
        possible_ops[opcode] = possible_ops.get(opcode, []) + [set(m)]

    op_map = {}
    used_ops = set()
    while len(op_map) < len(possible_ops):
        for k, v in possible_ops.items():
            op_set = None
            for op_results in v:
                if op_set is None:
                    op_set = op_results - used_ops
                else:
                    op_set &= op_results
            if len(op_set) != 1:
                print("failed to find unique op!! "+str(op_set))
            else:
                opcode = op_set.pop()
                op_map[k] = opcode
                used_ops |= {opcode}

    i += 2
    lines = lines[i:]
    ops = list(map(lambda l: util.parse_int_line(l), lines))
    regs = [0, 0, 0, 0]

    for op in ops:
        op_method = OPS[op_map[op[0]]]
        op_method(regs, op)

    print(regs)


def test_op(registers, op, expected):
    match = []
    for i in range(len(OPS)):
        reg = registers.copy()
        OPS[i](reg, op)
        if reg == expected:
            match.append(i)
    return match


def addr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] + registers[b]


def addi(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] + b


def mulr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] * registers[b]


def muli(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] * b


def banr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] & registers[b]


def bani(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] & b


def borr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] | registers[b]


def bori(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a] | b


def setr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = registers[a]


def seti(registers, op):
    [opcode, a, b, c] = op
    registers[c] = a


def gtir(registers, op):
    [opcode, a, b, c] = op
    registers[c] = 1 if a > registers[b] else 0


def gtri(registers, op):
    [opcode, a, b, c] = op
    registers[c] = 1 if registers[a] > b else 0


def gtrr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = 1 if registers[a] > registers[b] else 0


def eqir(registers, op):
    [opcode, a, b, c] = op
    registers[c] = 1 if a == registers[b] else 0


def eqri(registers, op):
    [opcode, a, b, c] = op
    registers[c] = 1 if registers[a] == b else 0


def eqrr(registers, op):
    [opcode, a, b, c] = op
    registers[c] = 1 if registers[a] == registers[b] else 0


OPS = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

if __name__ == '__main__':
    main()
