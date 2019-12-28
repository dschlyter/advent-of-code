import sys
from collections import defaultdict
from queue import Queue

import util

sys.setrecursionlimit(15000)
# sys.settrace(lambda x, y, z: print(x, y, z))


def part1(lines):
    # TODO try pysnoop

    portals = defaultdict(lambda: [])
    portal_lookup = {}
    start = None

    for i, line in enumerate(lines):
        for j, c in enumerate(line):
            for (i2, j2) in [(i+1, j), (i, j+1)]:
                if i2 < len(lines) and j2 < len(lines[i2]):
                    if 'A' <= lines[i][j] and 'A' <= lines[i2][j2]:
                        key = lines[i][j] + lines[i2][j2]
                        portals[key] = portals[key] + [(i, j), (i2, j2)]
                        if key == 'AA':
                            start = [(i, j), (i2, j2)]

    for code, pos_list in portals.items():
        for pos in pos_list:
            portal_lookup[pos] = code

    shortest_dist = None

    q = Queue()
    visited = set()

    for p in start:
        q.put((p, -1))

    while not q.empty():
        p, dist = q.get()
        if p in visited:
            continue
        visited.add(p)

        # print(p, dist)

        y, x = p
        for np in [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]:
            ny, nx = np
            m = util.get_in(lines, ny, nx)
            if m == '.':
                q.put((np, dist+1))
            elif portal_code := portal_lookup.get(np):
                for (y, x) in portals[portal_code]:
                    for np in [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]:
                        ny, nx = np
                        m = util.get_in(lines, ny, nx)
                        if m == '.':
                            q.put((np, dist+1))
                if portal_code == 'ZZ' and shortest_dist is None:
                    shortest_dist = dist

    print("part 1 answer", shortest_dist)


def part2(lines):
    portals = defaultdict(lambda: [])
    portal_lookup = {}
    start = None

    for i, line in enumerate(lines):
        for j, c in enumerate(line):
            for (i2, j2) in [(i+1, j), (i, j+1)]:
                if i2 < len(lines) and j2 < len(lines[i2]):
                    if 'A' <= lines[i][j] and 'A' <= lines[i2][j2]:
                        key = lines[i][j] + lines[i2][j2]
                        portals[key] = portals[key] + [(i, j), (i2, j2)]
                        if key == 'AA':
                            start = [(i, j), (i2, j2)]

    for code, pos_list in portals.items():
        for pos in pos_list:
            portal_lookup[pos] = code

    shortest_dist = None

    q = Queue()
    visited = set()

    for p in start:
        q.put((p, 0, -1))

    width = len(lines[0])
    height = len(lines[0])

    while not q.empty() and shortest_dist is None:
        p, level, dist = q.get()
        if (p, level) in visited:
            continue
        visited.add((p, level))

        if level > 100:
            print("level exceeded")
            continue

        # print(p, level, dist)

        y, x = p
        for np in [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]:
            ny, nx = np
            m = util.get_in(lines, ny, nx)
            if m == '.':
                q.put((np, level, dist+1))
            elif portal_code := portal_lookup.get(np):
                outer = ny <= 2 or ny >= height-2 or nx <= 2 or nx >= width-2
                next_level = (level-1) if outer else (level+1)
                # print(portal_code, np, outer)
                if next_level >= 0:
                    for (y, x) in portals[portal_code]:
                        for np in [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]:
                            if np == p:
                                continue
                            ny, nx = np
                            m = util.get_in(lines, ny, nx)
                            if m == '.':
                                q.put((np, next_level, dist+1))
                if portal_code == 'ZZ' and level == 0 and shortest_dist is None:
                    shortest_dist = dist

    print("part 2 answer", shortest_dist)


if __name__ == '__main__':
    parts, inputs = util.parse_args(__file__)
    # inputs = ["input/day20_test1.txt"]

    for input_file in inputs:
        print("Running", input_file)
        if 1 in parts:
            lines = util.read_input(input_file)
            part1(lines)
        if 2 in parts:
            lines = util.read_input(input_file)
            part2(lines)
