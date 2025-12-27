import util
import sys

from util import get

def main():
    TEST = "input/day5_test.txt"
    MAIN = "input/day5.txt"

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

def p1(lines):
    ans = 0

    range_lines, elem_lines = lines[:lines.index("")], lines[lines.index("") + 1:]

    ranges = []
    for line in range_lines:
        start, end = line.split("-")
        ranges.append((int(start), int(end)))

    # O(n^2) - could be opted with binary search, but try easy mode first
    for e in elem_lines:
        for start, end in ranges:
            val = int(e)
            if start <= val <= end:
                ans += 1
                break
    
    print("Part 1:", ans)

def p2(lines):
    ans = 0
    range_lines, elem_lines = lines[:lines.index("")], lines[lines.index("") + 1:]

    ranges = []
    for line in range_lines:
        start, end = line.split("-")
        ranges.append((int(start), int(end)))
    ranges = sorted(ranges)

    tail = 0
    for s, e in ranges:
        ans += max(0, e - max(s - 1, tail))
        tail = max(tail, e)

    print("Part 2:", ans)

if __name__ == "__main__":
    main()