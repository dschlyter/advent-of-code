import sys
from collections import defaultdict
from queue import Queue, PriorityQueue
from typing import List

import util

sys.setrecursionlimit(15000)


def part1(lines: List[str]):
    adj = ((-1, 0), (0, -1), (0, 1), (1, 0))

    m = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            m[(y, x)] = c

    seen = set()

    while True:
        state = set()
        for k, v in m.items():
            state.add((k, v))
        if state in seen:
            break
        seen.add(frozenset(state))

        new_map = {}
        for y in range(5):
            for x in range(5):
                state = m[(y, x)]
                count = 0
                for (yd, xd) in adj:
                    if m.get((y + yd, x + xd)) == '#':
                        count += 1
                if state == '#' and count != 1:
                    state = '.'
                elif state == '.' and (count == 1 or count == 2):
                    state = '#'
                new_map[(y, x)] = state

        m = new_map

    score = 0
    for y in range(5):
        for x in range(5):
            i = 5 * y + x
            if m[(y, x)] == '#':
                score += 2 ** i

    print("part 1 answer", score)


def part2(lines: List[str]):
    adj = ((-1, 0), (0, -1), (0, 1), (1, 0))

    m = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            m[(0, y, x)] = c

    seen = set()

    for minute in range(200):
        state = set()
        for pos, v in m.items():
            state.add((pos, v))
        if state in seen:
            break
        seen.add(frozenset(state))

        new_map = {}
        for level in range(-200, 200):
            for y in range(5):
                for x in range(5):
                    if y == 2 and x == 2:
                        # center space does not exist
                        continue

                    state = m.get((level, y, x), '.')
                    count = 0
                    for (yd, xd) in adj:
                        if m.get((level, y + yd, x + xd)) == '#':
                            count += 1
                    for (level_a, ya, xa) in level_adj(level, y, x):
                        if m.get((level_a, ya, xa)) == '#':
                            count += 1

                    if state == '#' and count != 1:
                        state = '.'
                    elif state == '.' and (count == 1 or count == 2):
                        state = '#'

                    new_map[(level, y, x)] = state

        m = new_map

    score = len([v for v in m.values() if v == '#'])

    # 107 too low
    print("part 2 answer", score)


def level_adj(level, y, x):
    if y == 0:
        yield (level-1, 1, 2)
    if y == 4:
        yield (level-1, 3, 2)
    if x == 0:
        yield (level-1, 2, 1)
    if x == 4:
        yield (level-1, 2, 3)

    if y == 2:
        if x == 1:
            for a in [(level+1, y, 0) for y in range(5)]:
                yield a
        if x == 3:
            for a in [(level+1, y, 4) for y in range(5)]:
                yield a

    if x == 2:
        if y == 1:
            for a in [(level+1, 0, x) for x in range(5)]:
                yield a
        if y == 3:
            for a in [(level+1, 4, x) for x in range(5)]:
                yield a


if __name__ == '__main__':
    util.main(__file__, part1, part2)
