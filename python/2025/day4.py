import util
import sys

from util import get

def main():
    if len(sys.argv) > 1:
        run(sys.argv[1])
    else:
        run("input/day4_test.txt")
        run("input/day4.txt")

def run(in_file):
    print("=== Running", in_file, "===")
    lines = util.read_input(in_file)
    p1(lines)
    p2(lines)

def p1(lines):
    ans = 0
    grid = [list(s) for s in lines]
    accessible = set()

    for r in range(len(grid)):
        for c in range(len(grid[r])):
            adj = 0
            if grid[r][c] != "@":
                continue
            for rd in range(-1, 2):
                for cd in range(-1, 2):
                    if abs(rd) + abs(cd) == 0:
                        continue
                    nr = r + rd
                    nc = c + cd
                    if get(get(grid, nr, []), nc, ".") == "@":
                        adj += 1
            if adj < 4:
                accessible.add((r, c))
    ans = len(accessible)
    print("Part 1:", ans)

def p2(lines):
    ans = 0
    grid = [list(s) for s in lines]

    while True:
        accessible = set()
        for r in range(len(grid)):
            for c in range(len(grid[r])):
                adj = 0
                if grid[r][c] != "@":
                    continue
                for rd in range(-1, 2):
                    for cd in range(-1, 2):
                        if abs(rd) + abs(cd) == 0:
                            continue
                        nr = r + rd
                        nc = c + cd
                        if get(get(grid, nr, []), nc, ".") == "@":
                            adj += 1
                if adj < 4:
                    accessible.add((r, c))
        if accessible:
            for r, c in accessible:
                grid[r][c] = "."
                ans += 1
        else:
            break
    print("Part 2:", ans)

if __name__ == "__main__":
    main()