import queue
import util
import sys
from collections import defaultdict
from functools import reduce
from operator import add, mul

from util import get

sys.setrecursionlimit(1_000_000)

def p1(lines):
    ans = 0
    
    points = [(int(x), int(y)) for line in lines for x, y in [line.split(",")]]

    for p1 in points:
        for p2 in points:
            if p1 == p2:
                continue
            ans = max(ans, (abs(p1[0] - p2[0]) + 1) * (abs(p1[1] - p2[1]) + 1))

    print("Part 1:", ans)

# Notes:
# There are at least 125M points in the shape for my input - flood fill seems pointless
# There are no self-intersections in my input
# No line is of length 1 - nasty edge case for outside set
def p2(lines):
    ans = 0
    
    lines = list(reversed(lines))

    points = [(int(x), int(y)) for line in lines for x, y in [line.split(",")]]
    point_set = set(points)

    # 1. In order to detect "outside" vs "inside" we need to determine the winding direction
    directions = [(1,0), (0, 1), (-1, 0), (0,-1)]
    def get_dir(p1, p2):
        if p1[0] == p2[0]:
            return 1 if p2[1] > p1[1] else 3
        else:
            return 0 if p2[0] > p1[0] else 2

    turn_sum = 0
    for i in range(len(points)):
        p0 = points[(i-1) % len(points)]
        p1 = points[i]
        p2 = points[(i+1) % len(points)]
        turn = (get_dir(p1, p2) - get_dir(p0, p1)) % 4
        # normalize to -1 or 1
        turn = turn - 4 if turn >= 2 else turn
        turn_sum += turn
    print("Turn sum:", turn_sum)

    if turn_sum == 4:
        outside_rot = -1
    elif turn_sum == -4:
        outside_rot = 1
    else:
        raise Exception("Invalid turn sum")

    # 2. Generate outside contour by walking around the shape
    line_set = set()
    contour = set()
    for i in range(len(points)):
        p1 = points[i]
        p2 = points[(i+1) % len(points)]
        for x in range(min(p1[0], p2[0]), max(p1[0], p2[0]) + 1):
            for y in range(min(p1[1], p2[1]), max(p1[1], p2[1]) + 1):
                line_set.add((x, y))
                line_dir = get_dir(p1, p2)
                out = util.add((x, y), directions[(line_dir + outside_rot) % 4])
                if out in point_set:
                    print("WARNING: Point collision in outside set")
                contour.add(out)
    contour = contour - line_set
    print(len(contour), "points in outside set")
    if len(contour) < 100:
        util.print_states(contour, ".", point_set, "#")


    # 3. Check all pairs of points, and validate rectangle does not intersect contour 
    # Runtime 1:45 - so not the most efficient
    def is_valid(p1, p2):
        min_x = min(p1[0], p2[0])
        max_x = max(p1[0], p2[0])
        min_y = min(p1[1], p2[1])
        max_y = max(p1[1], p2[1])
        corners = [(min_x, min_y), (min_x, max_y), (max_x, max_y), (max_x, min_y)]
        for i in range(4):
            s = corners[i]
            e = corners[(i+1) % 4]
            for x in range(min(s[0], e[0]), max(s[0], e[0]) + 1):
                for y in range(min(s[1], e[1]), max(s[1], e[1]) + 1):
                    if (x, y) in contour:
                        return False
        return True

    candidates = []
    for i, p1 in enumerate(points):
        for p2 in points[i+1:]:
            area = (abs(p1[0] - p2[0]) + 1) * (abs(p1[1] - p2[1]) + 1)
            candidates.append((area, p1, p2))
    candidates = sorted(candidates, key=lambda x: x[0], reverse=True)
    print(len(candidates), "candidates to check")

    for i, c in enumerate(candidates):
        if i % 1000 == 0:
            print(f"{i / len(candidates) * 100:.2f} % checked")
        area, p1, p2 = c
        if is_valid(p1, p2):
            ans = area
            break

    print("Part 2:", ans)

def main():
    TEST = "input/day9_test.txt"
    MAIN = "input/day9.txt"

    if len(sys.argv) > 1:
        if sys.argv[1] == "1":
            run(TEST)
        elif sys.argv[1] == "1":
            run(MAIN)
        else:
            run(sys.argv[1])
    else:
        run(TEST)
        run(MAIN)

def run(in_file):
    print("=== Running", in_file, "===")
    lines = util.read_input(in_file)
    p1(lines)
    p2(lines)

if __name__ == "__main__":
    main()