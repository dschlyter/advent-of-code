import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day18{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    # problem1_old()
    problem2()


dirs = {
    "R": (0, 1),
    "L": (0, -1),
    "D": (1, 0),
    "U": (-1, 0),
}


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    inst = []
    for l in lines:
        d, dist, _ = l.split(" ")
        inst.append((d, int(dist)))

    g = make_lines(inst)
    r = calc_size(g)
    print(r)


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    inst = []
    for l in lines:
        _, _, code = l.split(" ")
        code = code.replace("#", "").replace("(", "").replace(")", "")
        d = "RDLU"[int(code[-1])]
        # parse hex number
        dist = int(code[:-1], 16)
        print(d, dist)
        inst.append((d, dist))
    
    g = make_lines(inst)
    r = calc_size(g)
    print(r)


def make_lines(instructions):
    grid_lines = {
        "R": [],
        "L": [],
        "D": [],
        "U": [],
    }
    pos = 0, 0
    
    for dir, dist in instructions:
        d = dirs[dir]
        new_pos = pos[0] + d[0] * dist, pos[1] + d[1] * dist
        grid_lines[dir].append((pos, new_pos))
        pos = new_pos

    if pos != (0, 0):
        raise Exception("sanity check: not back at origin")

    return grid_lines


# match U lines against D lines to calc size
def calc_size(grid_lines):
    # convert L and R lines to small blockers
    blockers = []
    for l in grid_lines["L"] + grid_lines["R"]:
        y, x = min(l[0][0], l[1][0]), min(l[0][1], l[1][1])
        block_pos = (y, x+1)
        blockers.append((block_pos, block_pos))
    blockers = blockers + grid_lines["D"]
    # sort by x for lookup
    blockers = sorted(blockers, key=lambda x: x[0][1])
    # print("blockers", blockers)

    area = 0
    for l in grid_lines["U"]:
        area += area_size(l, blockers)

    # print("area", area)

    for lines in grid_lines.values():
        for l in lines:
            line_size = abs(l[0][0] - l[1][0]) + abs(l[0][1] - l[1][1])
            print(l, line_size)
            area += line_size
    
    return area


def area_size(line, blockers):
    print("lines size", line)
    x = line[0][1]
    ys = min(line[0][0], line[1][0])
    ye = max(line[0][0], line[1][0])

    for b in blockers:
        x2 = b[0][1]
        # TODO binary search opt ?
        if x2 <= x:
            continue

        y2s = min(b[0][0], b[1][0])
        y2e = max(b[0][0], b[1][0])
        if y2s > ye or y2e < ys:
            continue

        ret = (min(ye, y2e) - max(ys, y2s) + 1) * (x2 - x - 1)
        print("area", line, b, min(ye, y2e), max(ys, y2s), (x2 - x - 1), ret)
        before = max(0, y2s - ys)
        after = max(0, ye - y2e)

        if before > 0:
            ret += area_size([(ys, x), (ys + before - 1, x)], blockers)
        if after > 0:
            ret += area_size([(ye - after + 1, x), (ye, x)], blockers)

        return ret


# complicated and too slow for #2
def problem1_old():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    grid = defaultdict(lambda: defaultdict(lambda: "."))
    y, x = 0, 0
    grid[y][x] = "#"

    for l in lines:
        d, dist, _ = l.split(" ")
        for i in range(int(dist)):
            # grid[y][x] = "#"
            grid[y][x] = d
            yd, xd = dirs[d]
            x, y = x + xd, y + yd

    for y in range(-100, 100):
        for x in range(-100, 100):
            print(grid[y][x], end="")
        print()

    ymin, ymax = min(grid.keys()), max(grid.keys())
    xmin = min([min(row.keys()) for row in grid.values()])
    xmax = max([max(row.keys()) for row in grid.values()])
    print("bounds", ymin, ymax, xmin, xmax, (ymax - ymin) * (xmax - xmin))

    inside, outside = set(), set()
    for y in range(min(grid.keys()), max(grid.keys()) + 1):
        print(y)
        for x in range(min(grid[y].keys()), max(grid[y].keys()) + 1):
            is_outside = False

            if (y, x) in inside or (y, x) in outside:
                continue

            new_visited = set()
            q = Queue()
            q.put((y, x))
            while not q.empty():
                k = q.get()
                ny, nx = k

                if grid[ny][nx] != ".":
                    continue

                if k in new_visited:
                    continue
                new_visited.add(k)

                if ny < ymin or ny > ymax or nx < xmin or nx > xmax or k in outside:
                    is_outside = True
                    break

                for dy, dx in dirs.values():
                    q.put((ny + dy, nx + dx))

            if is_outside:
                outside = outside.union(new_visited)
            else:
                inside = inside.union(new_visited)

    path_size = len([c for row in grid.values() for c in row.values() if c == "#"])
    inner_size = len(inside)
    print("outside", len(outside))
    print(path_size, inner_size, path_size + inner_size)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
