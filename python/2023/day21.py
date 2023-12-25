import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day21{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [[c for c in l.strip()] for l in fp]

    for i in range(64):
        next_tiles = set()
        for y, l in enumerate(lines):
            for x, c in enumerate(l):
                if c == "S" or c == "O":
                    lines[y][x] = "."
                    next_tiles.add((x, y-1))
                    next_tiles.add((x, y+1))
                    next_tiles.add((x-1, y))
                    next_tiles.add((x+1, y))

        for x, y in next_tiles:
            if y < 0 or y >= len(lines) or x < 0 or x >= len(lines[y]):
                k = (x, y)
                continue
            if lines[y][x] == "#":
                continue
            lines[y][x] = "O"
            
    ans = 0
    for l in lines:
        print("".join(l))
        for c in l:
            if c == "O":
                ans += 1
    print(ans)


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]
    grid = [[c for c in l] for l in lines]

    sx, sy = None, None
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == "S":
                sx, sy = x, y
                grid[y][x] = "."

    reach_in(grid, 6, (sx, sy))
    reach_in(grid, 10, (sx, sy))
    reach_in(grid, 50, (sx, sy))
    # reach_in(grid, 100, (sx, sy)) # wrong here - 6509
    # reach_in(grid, 500, (sx, sy)) # this is wrong, but others ok, wtf ?? - 166959
    # reach_in(grid, 1000, (sx, sy))
    # reach_in(grid, 5000, (sx, sy))
    reach_in(grid, 26501365, (sx, sy))


def reach_in(grid, steps, start):
    sx, sy = start
    # assume grid is square
    l = len(grid)

    # calculate the main grid, and how we enter other grids
    plots, main_spawns = search_grid(grid, [(sx, sy, 0)], steps)
    print("main grid count", plots)

    # calculate grids going straight in four directions
    def axis_norm(spawns, axis, value):
        filtered = [s for s in spawns if s[axis] == value]
        return [(x % l, y % l, t) for x, y, t in filtered]

    for axis, value in [(0, -1), (0, l), (1, -1), (1, l)]:
        direction_spawns = axis_norm(main_spawns, axis, value)
        while True:
            if len(direction_spawns) == 0:
                break

            new_plots, new_spawns = search_grid(grid, direction_spawns, steps)
            plots += new_plots
            new_spawns = axis_norm(new_spawns, axis, value)

            # when we keep spawning our own pattern, we have a repeat - optimize away the repeats - alternating odd/even
            if [(x, y) for x, y, t in direction_spawns] == [(x, y) for x, y, t in new_spawns] and new_spawns[0][2] + l * 2 < steps:
                start_time = min([t for x, y, t in new_spawns]) 
                iteration_time = start_time - min([t for x, y, t in direction_spawns])
                repeat_times = (steps - start_time) // iteration_time // 2 * 2 - 2
                if repeat_times > 0:
                    this_plots, _ = search_grid(grid, new_spawns, steps)
                    plots += (repeat_times // 2) * this_plots
                    plots += (repeat_times // 2) * new_plots
                    new_spawns = [(x, y, t + repeat_times * iteration_time) for x, y, t in new_spawns]
                print("optimized", repeat_times, "repeats on straight axix", axis, value)
            else:
                pass

            direction_spawns = new_spawns

    # calculate diagonal spreads - for these we will always start at the edge, and for every layer there is one more grid
    for x, y in ((-1, -1), (-1, l), (l, -1), (l, l)):
        if not main_spawns:
            break

        steps_start = 90000
        for (sx, sy, t) in main_spawns:
            steps_start = min(steps_start, abs(sx - x) + abs(sy - y) + t)
        print("fastest distance to square", x, y, steps_start)

        diagonal_grids = 0
        sx, sy = x % l, y % l
        while steps_start < steps:
            # diagonal increases by size 1 each time
            diagonal_grids += 1
            new_plots, _ = search_grid(grid, [(sx, sy, steps_start)], steps)
            steps_start += l
            plots += diagonal_grids * new_plots

            # optimize by reusing the same grid result for full grids - alternating odd/even
            if steps_start + l * 3 < steps:
                opt_steps = 0
                this_plots, _ = search_grid(grid, [(sx, sy, steps_start)], steps)
                next_plots, _ = search_grid(grid, [(sx, sy, steps_start + l)], steps)
                while steps_start + l * 3 < steps:
                    diagonal_grids += 1
                    plots += diagonal_grids * this_plots
                    diagonal_grids += 1
                    plots += diagonal_grids * next_plots
                    steps_start += l * 2
                    opt_steps += 2
                print("optimized", opt_steps, "diagonal layers for axis", x, y)


    print()
    print("in", steps, "steps, can reach", plots)
    print()

    
# return number of plots after max steps, and spawns into neighboring grids
def search_grid(grid, start_pattern, max_steps):
    seen = set()
    active = list()
    spawns = []

    i = min([t for x, y, t in start_pattern])
    while i < max_steps:
        for x, y, t in start_pattern:
            if i == t:
                active.append((x, y))

        next_candidates = set()
        for x, y in active:
            next_candidates.add((x, y-1))
            next_candidates.add((x, y+1))
            next_candidates.add((x-1, y))
            next_candidates.add((x+1, y))

        next_active = []
        for x, y in next_candidates:
            if y < 0 or y >= len(grid) or x < 0 or x >= len(grid[y]):
                turn = i + 1
                new_spawn = True
                for sx, sy, t in spawns:
                    if abs(sx - x) + abs(sy - y) <= (turn - t):
                        new_spawn = False
                        break
                if new_spawn:
                    spawns.append((x, y, turn))
                continue
            if grid[y][x] == "#":
                continue
            next_active.append((x, y))

        key = frozenset(next_active)
        if len(next_active) > 0 and key in seen:
            steps_left = max(0, max_steps - i - 2)
            # advance an even number of steps since we are repeating
            i += (steps_left // 2) * 2
        seen.add(key)

        active = next_active
        i += 1
    
    return len(active), spawns


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
