import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day23{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    # problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        grid = [l.strip() for l in fp]

    r = find_path(grid, (0, 1), set())
    print(r)


def problem2():
    with open(infile) as fp:
        grid = [[c for c in l.strip()] for l in fp]

    chokepoints = set()
    # algo is bugged - there actually isn't any chokepoints it seems
    find_articuation_points(grid, (0, 0), (0, 1), 0, dict(), dict(), chokepoints)
    print(chokepoints)


    fakepoints = set()
    for y, x in sorted(chokepoints):
        grid[y][x] = "#"
        reach = can_reach_goal(grid, (0, 1), set())
        print("choke", y, x, "is", "Fake" if reach else "Real")
        if reach:
            fakepoints.add((y, x))
        grid[y][x] = "."

    for y, l in enumerate(grid):
        for x, c in enumerate(l):
            if (y, x) in fakepoints:
                print("F", end="")
            elif (y, x) in chokepoints:
                print("O", end="")
            else:
                print(c, end="")
        print()

    # r = find_path(grid, (0, 1), set(), unlimied=True)
    # print(r)


def find_articuation_points(grid, parent, pos, t, disc, low, ap):
    y, x = pos
    if y == len(grid) - 1 and x == len(grid[0]) - 2:
        pass
    if y >= len(grid) or x >= len(grid[0]) - 1 or grid[y][x] == "#":
        pass
    elif pos in disc:
        low[parent] = min(low[parent], disc[pos])
    else:
        disc[pos] = t
        low[pos] = t
        t += 1

        # traverse to update low values
        for dy, dx in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
            new_pos = (y + dy, x + dx)
            if new_pos == parent:
                continue
            t = find_articuation_points(grid, pos, new_pos, t, disc, low, ap)

            low[pos] = min(low[pos], low.get(new_pos, low[pos]))
            if low.get(new_pos, -1) >= disc[pos]:
                ap.add(pos)

    return t


best = 0


def find_path(grid, pos, visited, unlimied=False):
    y, x = pos

    if y == len(grid) - 1 and x == len(grid[0]) - 2:
        global best
        best = max(len(visited), best)
        # hacky solve for part2 - just run long enough to find the longest path in max
        print("Found path with len", len(visited), best)
        return len(visited)
    if pos in visited:
        return None
    if grid[y][x] == "#":
        return None
    
    visited.add(pos)
    if grid[y][x] == ".":
        dirs = "^>v<"
    else:
        dirs = grid[y][x]

    best_path = None
    for c in dirs:
        new_pos = {
            "^": (y - 1, x),
            ">": (y, x + 1),
            "v": (y + 1, x),
            "<": (y, x - 1),
        }[c]
        r = find_path(grid, new_pos, visited, unlimied)
        if r is not None and (best_path is None or r > best_path):
            best_path = r
    visited.remove(pos)

    return best_path


def find_path2(grid, start, pos, goals, visited=set()):
    y, x = pos

    if pos in goals:
        return {pos: len(visited)}
    if pos in visited:
        return None
    if grid[y][x] == "#":
        return None

    visited.add(pos)
    if grid[y][x] == ".":
        dirs = "^>v<"
    else:
        dirs = grid[y][x]

    best_path = None
    for dy, dx in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
        new_pos = (y + dy, x + dx)
        new_goals = find_path(grid, new_pos, visited)
        if r is not None and (best_path is None or r > best_path):
            best_path = r
    visited.remove(pos)

    return best_path


def can_reach_goal(grid, pos, visited):
    visited2 = set()

    q = Queue()
    q.put(pos)
    while not q.empty():
        pos = q.get()
        y, x = pos

        if y == len(grid) - 1 and x == len(grid[0]) - 2:
            return True
        if pos in visited or pos in visited2:
            continue
        if grid[y][x] == "#":
            continue
        visited2.add(pos)

        for dy, dx in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
            q.put((y + dy, x + dx))

    return False


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
