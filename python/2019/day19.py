import itertools
import queue
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue
from typing import List

import util
from intcode import IntCode

sys.setrecursionlimit(15000)

import pysnooper

@pysnooper.snoop()
def part1(lines: List[str]):
    program = list(map(int, lines[0].split(",")))

    count = 0
    for x in range(50):
        for y in range(50):

            computer = IntCode(program)
            computer.input_instructions.append(x)
            computer.input_instructions.append(y)
            computer.run()
            if computer.output_values.popleft() == 1:
                count += 1

    print("part 1 answer", count)


def part2(lines: List[str]):
    program = list(map(int, lines[0].split(",")))

    size = 2500

    min_x = 0

    beam = set()
    for y in range(size):
        found_beam = False
        for x in range(size):
            if x < min_x:
                continue

            computer = IntCode(program)
            computer.input_instructions.append(x)
            computer.input_instructions.append(y)
            computer.run()

            if computer.output_values.popleft() == 1:
                beam.add((y, x))
                found_beam = True
            else:
                if not found_beam:
                    min_x = min(x, min_x)
                else:
                    print(x, "beam size", x - min_x)
                    break

    print_size = min(size, 100)
    for y in range(print_size):
        for x in range(print_size):
            if (y, x) in beam:
                print("#", end='')
            else:
                print(".", end='')
        print()

    best = None
    for y in range(size):
        for x in range(size):
            if best and x+y > best[0] + best[1]:
                break

            in_beam = True
            for yi in range(y, y+100):
                for xi in range(x, x+100):
                    if (yi, xi) not in beam:
                        in_beam = False
                        break
                if not in_beam:
                    break

            if in_beam and (not best or x+y < best[0] + best[1]):
                best = y, x

    y, x = best
    print(best)

    # 580109 is too low :(
    # 1090058 is too low :(

    print("part 2 answer", x * 10000 + y)


if __name__ == '__main__':
    util.main(__file__, part1, lambda x: 0)
