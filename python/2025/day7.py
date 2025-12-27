import util
import sys
from collections import defaultdict
from functools import reduce
from operator import add, mul

from util import get

def p1(lines):
    ans = 0

    beams = {lines[0].index("S")}
    for line in lines[1:]:
        new_beams = set()
        for i, c in enumerate(line):
            if i in beams:
                if c == "^":
                    new_beams.add(i-1)
                    new_beams.add(i+1)
                    ans += 1
                else:
                    new_beams.add(i)
        beams = new_beams

    print("Part 1:", ans)

def p2(lines):

    beams = {lines[0].index("S"): 1}
    for line in lines[1:]:
        new_beams = defaultdict(int)
        for i, c in enumerate(line):
            if i in beams:
                if c == "^":
                    beam = beams[i]
                    new_beams[i-1] += beam
                    new_beams[i+1] += beam
                else:
                    new_beams[i] += beams[i]
        beams = new_beams

    print("Part 2:", sum(beams.values()))

def main():
    TEST = "input/day7_test.txt"
    MAIN = "input/day7.txt"

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