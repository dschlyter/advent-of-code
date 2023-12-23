import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

infile = 'input/day10.txt'

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


dirs = {
    "N": (-1, 0),
    "S": (1, 0),
    "E": (0, 1),
    "W": (0, -1),
}
reverse = {
    "N": "S",
    "S": "N",
    "E": "W",
    "W": "E",
}
clockwise = "NESW"


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    sy, sx = None, None
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            if lines[y][x] == "S":
                sy, sx = y, x

    best_size = 0
    best_dir = None
    for d in "NSEW":
        dy, dx = dirs[d]
        visited = {(sy, sx)}
        loop_size = dfs(sy + dy, sx + dx, d, lines, visited, 1)
        if loop_size > best_size:
            best_size = loop_size
            best_dir = d
        print(d, loop_size)
    print("Best dir", best_dir, best_size, best_size / 2)


def dfs(y, x, d, lines, visited, count):
    if lines[y][x] == "S":
        return count

    if lines[y][x] == ".":
        if count > 1:
            raise Exception("sanity check: Pipe should not go into ground")
        return 0

    connect_dirs = {
        "|": "NS",
        "-": "EW",
        "L": "NE",
        "J": "NW",
        "7": "SW",
        "F": "SE",
    }[lines[y][x]]

    if reverse[d] not in connect_dirs:
        print("Not connected to source")
        return count

    if (y, x) in visited:
        return 0
    visited.add((y, x))

    res = []
    for d in connect_dirs:
        dy, dx = dirs[d]
        l = dfs(y + dy, x + dx, d, lines, visited, count + 1)
        res.append(l)
    return max(res)


def problem2():
    print()
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    sy, sx = None, None
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            if lines[y][x] == "S":
                sy, sx = y, x

    for d in "NSEW":
        dy, dx = dirs[d]
        visited = {(sy, sx)}
        loop_size = dfs(sy + dy, sx + dx, d, lines, visited, 1)

        if loop_size <= 1:
            continue

        visited_left = set()
        visited_right = set()
        l, r = dfs2(sy + dy, sx + dx, d, lines, visited, visited_left, visited_right)

        if visited_left.intersection(visited_right):
            raise Exception("sanity check: Left and right should not intersect")

        for y in range(len(lines)):
            for x in range(len(lines[y])):
                if (y, x) in visited_left:
                    print("L", end="")
                elif (y, x) in visited_right:
                    print("R", end="")
                elif (y, x) in visited:
                    print("P", end="")
                else:
                    print(lines[y][x], end="")
            print()
        print(d, loop_size, l, r)

    # 31 wronk !!
    # 41 wrong
    # 486 too low


def dfs2(y, x, d, lines, visited, visited_left, visited_right):
    if lines[y][x] == "S":
        return 0, 0

    connect_dirs = {
        "|": "NS",
        "-": "EW",
        "L": "NE",
        "J": "NW",
        "7": "SW",
        "F": "SE",
    }[lines[y][x]]
    from_dir = reverse[d]
    next_dir = list(filter(lambda x: x != from_dir, connect_dirs))[0]

    if from_dir not in connect_dirs:
        print("Not connected to source")
        return -1, -1

    left_count, right_count = 0, 0

    i = clockwise.index(next_dir)
    while True:
        i = (i + 1) % len(clockwise)
        if clockwise[i] == from_dir:
            break
        d = dirs[clockwise[i]]
        left_count = plus(left_count, find_ground(y + d[0], x + d[1], lines, visited_left, visited))

    i = clockwise.index(from_dir)
    while True:
        i = (i + 1) % len(clockwise)
        if clockwise[i] == next_dir:
            break
        d = dirs[clockwise[i]]
        right_count = plus(right_count, find_ground(y + d[0], x + d[1], lines, visited_right, visited))

    dy, dx = dirs[next_dir]
    l, r = dfs2(y + dy, x + dx, next_dir, lines, visited, visited_left, visited_right)
    return plus(l, left_count), plus(r, right_count)



def plus(a, b):
    if a == -1 or b == -1:
        return -1
    return a + b


def find_ground(y, x, lines, visited, pipe_visited):
    ground_count = 0

    q = Queue()
    q.put((y, x))

    while not q.empty():
        y, x = q.get()
        if (y, x) in visited or (y, x) in pipe_visited:
            continue
        visited.add((y, x))

        if y < 0 or y >= len(lines) or x < 0 or x >= len(lines[y]):
            # connected to the open world
            return -1

        ground_count += 1

        for d in clockwise:
            dy, dx = dirs[d]
            q.put((y + dy, x + dx))

    return ground_count


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
