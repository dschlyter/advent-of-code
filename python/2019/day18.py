import sys
from collections import defaultdict
from queue import Queue, PriorityQueue
from typing import List

import fileinput
import heapq

import util

sys.setrecursionlimit(15000)


def part1(lines: List[str]):
    ans = 0

    q = Queue()
    visited = defaultdict(lambda: [])

    all_keys = set()

    sx, sy = 0, 0
    for y, line in enumerate(lines):
        if '@' in line:
            sy = y
            sx = line.index('@')
        for block in line:
            if 'a' <= block <= 'z':
                all_keys.add(block)

    q.put(((sy, sx), 0, set()))

    states = 0

    while not q.empty():
        pos, dist, keys = q.get()
        y, x = pos

        block = lines[y][x]

        if block == '#':
            continue

        is_visited = False
        for keyset in visited[pos]:
            if keys.issubset(keyset):
                is_visited = True
        if is_visited:
            continue
        visited[pos].append(keys)

        if 'A' <= block <= 'Z':
            if block.lower() not in keys:
                continue

        if 'a' <= block <= 'z':
            keys = keys | {block.lower()}
            if len(keys) == len(all_keys):
                ans = dist
                break

        for adj in [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]:
            q.put((adj, dist+1, keys))

        states += 1

    print("states explored", states)
    print("part 1 answer", ans)


def part2(lines: List[str]):
    ans = None

    all_keys = set()

    start_pos = []
    for y, line in enumerate(lines):
        for x, block in enumerate(line):
            if 'a' <= block <= 'z':
                all_keys.add(block)
            if block == '@':
                start_pos.append((y, x))

    print("start pos", start_pos)

    max_keys = 0

    q1 = PriorityQueue()
    visited1 = set()
    # state: (moves, positions, keys, history (for debugging))
    q1.put((0, start_pos, frozenset(), []))

    while not q1.empty() and ans is None:
        moves, positions, keys, hist = q1.get()

        # GAAAAAAAH - this must be on pop and not push - OBVIOUSLY
        if keys == all_keys:
            print("history", hist)
            ans = moves
            break

        pos_set = (frozenset(positions),  frozenset(keys))
        if pos_set in visited1:
            continue
        visited1.add(pos_set)

        if len(keys) > max_keys:
            max_keys = len(keys)
            print(moves, "keys", len(keys), len(all_keys))

        for i, pos1 in enumerate(positions):
            for d, new_pos, new_keys in next_keys(lines, pos1, keys):
                pos_next = positions[:i] + [new_pos] + positions[i+1:]
                new_hist = hist + list(new_keys - keys)
                q1.put((moves + d, pos_next, frozenset(new_keys), new_hist))

    print("part 2 answer", ans)


def next_keys(lines, pos1, keys):
    ret = []

    q = Queue()
    visited = set()

    q.put((pos1, 0))

    while not q.empty():
        p, dist = q.get()
        y, x = p

        block = lines[y][x]

        if block == '#':
            continue

        if p in visited:
            continue
        visited.add(p)

        if block.isupper():
            if block.lower() not in keys:
                continue

        if block.islower() and block not in keys:
            new_keys = keys | {block.lower()}
            ret.append((dist, p, new_keys))
            continue

        for adj in [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]:
            q.put((adj, dist+1))

    return ret


if __name__ == '__main__':
    # util.main(__file__, part1, lambda: 1)
    util.main(__file__, lambda lines: None, part2, match="_p2_test")
