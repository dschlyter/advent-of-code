import queue
import math
import util
import sys
from collections import defaultdict 
from functools import reduce
from operator import add, mul

from util import get

from ortools.sat.python import cp_model

sys.setrecursionlimit(1_000_000)

def parse(lines):
    ret = []
    for l in lines:
        p = l.split(" ")
        lights = p[0][1:-1]
        buttons = []
        for p2 in p[1:-1]:
            conns = list(map(int, p2[1:-1].split(",")))
            buttons.append(conns)
        joltage = tuple(map(int, p[-1][1:-1].split(",")))

        ret.append((lights, buttons, joltage))

    return ret

def p1(lines):
    print("Part 1")
    ans = 0

    my_machines = parse(lines)

    for m in my_machines:
        lights, buttons, joltage = m
        mem = {}
        r = dp(lights, 0, buttons, mem)
        # print(r)
        if r >= len(buttons):
            raise Exception("No solution found")
        ans += r

    print("Part 1:", ans)

def dp(state, index, buttons, mem):
    key = (state, index)
    if key in mem:
        return mem[key]

    if index >= len(buttons):
        if all(c == "." for c in state):
            return 0
        else:
            return 9000

    ans = 0
    # try pressing button
    new_state = list(state)
    for b in buttons[index]:
        new_state[b] = "#" if new_state[b] == "." else "."
    press = dp("".join(new_state), index + 1, buttons, mem)
    no_press = dp(state, index + 1, buttons, mem)
    ans = min(press + 1, no_press)

    mem[key] = ans
    return ans

def p2(lines):
    ans = 0

    my_machines = parse(lines)

    for m in my_machines:
        lights, buttons, joltage = m

        model = cp_model.CpModel()

        model_vars = []
        for i, b in enumerate(buttons):
            mv = model.NewIntVar(0, max(joltage), "b" + str(i))
            model_vars.append(mv)

        for joltage_i, j in enumerate(joltage):
            model.Add(sum(model_vars[bi] for bi, button in enumerate(buttons) if joltage_i in button) == j)

        # model.Minimize(reduce(lambda a, b: a + b, model_vars))
        model.Minimize(sum(model_vars))

        solver = cp_model.CpSolver()
        status = solver.Solve(model)
        if status == cp_model.OPTIMAL:
            # for mv in model_vars:
                # print(mv, solver.Value(mv))
            presses = sum(solver.Value(mv) for mv in model_vars)
            print(presses)
            ans += presses
        else:
            print("Non optimal :(")

    print("Part 2:", ans)


def main():
    TEST = "input/day10_test.txt"
    MAIN = "input/day10.txt"

    if len(sys.argv) > 1:
        if sys.argv[1] == "1":
            run(TEST)
        elif sys.argv[1] == "2":
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