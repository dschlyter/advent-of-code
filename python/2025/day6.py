import util
import sys
from functools import reduce
from operator import add, mul

from util import get

def p1(lines):
    ans = 0

    for i, c in enumerate(lines[-1]):
        if c == " ":
            continue

        start_i = i
        end_i = i+1
        while end_i < len(lines[-1]) and lines[-1][end_i] == " ":
            end_i += 1

        op = c
        operands = []
        for line in lines[:-1]:
            operands.append(int(line[start_i:end_i]))
        
        operator = add if op == "+" else mul
        result = reduce(operator, operands)
        ans += result

    print("Part 1:", ans)

def p2(lines):
    ans = 0

    for i, c in enumerate(lines[-1]):
        if c == " ":
            continue

        start_i = i
        end_i = i+1
        while end_i < len(lines[-1]) and lines[-1][end_i] == " ":
            end_i += 1

        op = c
        operands = []
        for c in range(start_i, end_i):
            col_vals = ""
            for line in lines[:-1]:
                if line[c] != " ":
                    col_vals += line[c]
            if col_vals:
                operands.append(int(col_vals))
        
        operator = add if op == "+" else mul
        result = reduce(operator, operands)
        ans += result

    print("Part 2:", ans)

def main():
    TEST = "input/day6_test.txt"
    MAIN = "input/day6.txt"

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