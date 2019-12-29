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
    ans = None
    program = list(map(int, lines[0].split(",")))

    computers = []
    queues = []

    for i in range(50):
        c = IntCode(program)
        c.input_instructions.append(i)

        computers.append(c)
        queues.append(queue.Queue())

    while ans is None:
        for i in range(50):
            computers[i].run(suspend_on_input=True, suspend_on_output=True)

        for i in range(50):
            c = computers[i]
            if c.output_values:
                c.run(suspend_on_output=True)
                c.run(suspend_on_output=True)
                assert(len(c.output_values) == 3)

                dest = c.output_values.popleft()
                X = c.output_values.popleft()
                Y = c.output_values.popleft()
                if dest == 255:
                    ans = Y
                    break
                queues[dest].put((X, Y))

        for i in range(50):
            c = computers[i]
            q = queues[i]
            if q.empty():
                c.input_instructions.append(-1)
            else:
                X, Y = q.get()
                c.input_instructions.append(X)
                c.input_instructions.append(Y)

    print("part 1 answer", ans)


def part2(lines: List[str]):
    ans = None
    program = list(map(int, lines[0].split(",")))

    computers = []
    queues = []

    for i in range(50):
        c = IntCode(program)
        c.input_instructions.append(i)

        computers.append(c)
        queues.append(queue.Queue())

    last_nat = (-2, -2)
    NAT = (-1, -1)

    while ans is None:
        for i in range(50):
            computers[i].run(suspend_on_input=True, suspend_on_output=True)

        for i in range(50):
            c = computers[i]
            if c.output_values:
                c.run(suspend_on_output=True)
                c.run(suspend_on_output=True)
                assert(len(c.output_values) == 3)

                dest = c.output_values.popleft()
                X = c.output_values.popleft()
                Y = c.output_values.popleft()
                if dest == 255:
                    NAT = (X, Y)
                else:
                    queues[dest].put((X, Y))

        all_empty = True
        for i in range(50):
            if not queues[i].empty():
                all_empty = False

        if all_empty:
            if last_nat[1] == NAT[1]:
                ans = last_nat[1]
                break
            last_nat = NAT

            queues[0].put(NAT)

        for i in range(50):
            c = computers[i]
            q = queues[i]
            if q.empty():
                c.input_instructions.append(-1)
            else:
                X, Y = q.get()
                c.input_instructions.append(X)
                c.input_instructions.append(Y)

    print("part 2 answer", ans)


if __name__ == '__main__':
    util.main(__file__, part1, part2)
