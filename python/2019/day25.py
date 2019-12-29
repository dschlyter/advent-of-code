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
    computer = IntCode(program)

    commands = []
    replay = ['north', 'take festive hat', 'west', 'take sand', 'south', 'north', 'east', 'east', 'take prime number', 'west', 'south', 'east', 'north', 'take weather machine', 'north', 'take mug', 'south',
              'south', 'east', 'north', 'east', 'east', 'take astronaut ice cream', 'west', 'west', 'south', 'west', 'west', 'south', 'west', 'east', 'south', 'take mutex', 'south', 'take boulder', 'east', 'south']

    items = ['sand', 'astronaut ice cream', 'weather machine', 'festive hat', 'boulder', 'prime number', 'mug', 'mutex']

    for L in range(0, len(items)+1):
        for subset in itertools.combinations(items, L):
            for item in items:
                replay.append('drop '+item)
            for item in subset:
                replay.append('take '+item)
            replay.append('inv')
            replay.append('east')

    while not computer.halted:
        computer.run(suspend_on_input=True)
        for out in computer.output_values:
            print(chr(out), end='')
        computer.output_values.clear()

        if not replay:
            command = input('')
            commands.append(command)
        else:
            command = replay[0]
            replay = replay[1:]

        inputs = list(map(ord, command)) + [10]
        for inp in inputs:
            computer.input_instructions.append(inp)

        # print(commands)

    print("part 1 answer", ans)


def part2(lines: List[str]):
    ans = None

    print("part 2 answer", ans)


if __name__ == '__main__':
    util.main(__file__, part1, part2)
