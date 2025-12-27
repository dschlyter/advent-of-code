import util
import sys
from collections import defaultdict
from functools import reduce
from operator import add, mul

from util import get

def p1(lines):
    points = []
    for l in lines:
        points.append(tuple(map(int, l.split(","))))

    pairs = []
    for i in range(len(points)):
        for j in range(i):
            pairs.append((dist(points[i], points[j]), i, j))
    pairs.sort()

    conns = defaultdict(list)
    for i in range(10 if len(lines) < 30 else 1000):
        d, a, b = pairs[i]
        conns[a].append(b)
        conns[b].append(a)

    visited = set()
    def dfs(p):
        if p in visited:
            return
        visited.add(p)
        for i in conns[p]:
            dfs(i)
    
    sizes = []
    for i in range(len(points)):
        pre_size = len(visited)
        dfs(i)
        post_size = len(visited)
        if post_size > pre_size:
            sizes.append(post_size - pre_size)

    print("Part 1:", reduce(mul, sorted(sizes, reverse=True)[0:3]))

def p2(lines):
    points = []
    for l in lines:
        points.append(tuple(map(int, l.split(","))))

    pairs = []
    for i in range(len(points)):
        for j in range(i):
            pairs.append((dist(points[i], points[j]), i, j))
    pairs.sort()

    conn_count = 0
    root = {i: i for i in range(len(points))}

    def find(i):
        if root[i] != i:
            root[i] = find(root[i])
        return root[i]

    for pa in pairs:
        d, a, b = pa
        ar = find(a)
        br = find(b)
        if ar != br:
            root[ar] = br
            conn_count += 1
        if conn_count >= len(points) - 1:
            print("Part 2:", points[a][0] * points[b][0])
            break

def dist(a, b):
    return abs(a[0] - b[0]) ** 2 + abs(a[1] - b[1]) ** 2 + abs(a[2] - b[2]) ** 2

def main():
    TEST = "input/day8_test.txt"
    MAIN = "input/day8.txt"

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