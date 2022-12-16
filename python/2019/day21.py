import itertools
import queue
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue
from typing import List

import util
from intcode import IntCode

sys.setrecursionlimit(15000)


def part1(lines: List[str]):
    program = list(map(int, lines[0].split(",")))
    computer = IntCode(program)

    computer.input_ascii("NOT A T")
    computer.input_ascii("OR T J")
    computer.input_ascii("NOT B T")
    computer.input_ascii("OR T J")
    computer.input_ascii("NOT C T")
    computer.input_ascii("OR T J")
    computer.input_ascii("AND D J")
    computer.input_ascii("WALK")

    while True:
        computer.run(suspend_on_input=True)
        computer.print_ascii()

        if computer.halted:
            break

        cmd = input("")
        computer.input_ascii(cmd)

    print("part 1 answer", computer.output_values)


def part2(lines: List[str]):
    program = list(map(int, lines[0].split(",")))
    computer = IntCode(program)

    computer.input_ascii("NOT T T")
    computer.input_ascii("AND A T")
    computer.input_ascii("AND B T")
    computer.input_ascii("AND C T")
    computer.input_ascii("NOT T T")

    computer.input_ascii("AND D T")
    computer.input_ascii("AND H T")
    computer.input_ascii("OR T J")

    # always jump if next is hole
    computer.input_ascii("NOT A T")
    computer.input_ascii("OR T J")

    computer.input_ascii("RUN")

    while True:
        computer.run(suspend_on_input=True)
        computer.print_ascii()
        print("count", computer.count)

        if computer.halted:
            break

        cmd = input("")
        computer.input_ascii(cmd)

    print("part 2 answer", computer.output_values)


if __name__ == '__main__':
    util.main(__file__, part1, part2)
