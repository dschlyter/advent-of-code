import queue
import util
import sys
from collections import defaultdict 
from functools import reduce
from operator import add, mul

from util import get

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

        cnt = dict()
        for b in buttons:
            for bi in b:
                cnt[bi] = cnt.get(bi, 0) + 1
        print(sorted(cnt.items()))

        mem = {tuple([0]*len(joltage)): 0}
        s_buttons = sorted(buttons, key=lambda b: (b[0], -len(b)))
        print("Sum joltage:", sum(joltage))
        r = dp2(joltage, s_buttons, mem)
        print(r)
        if r >= sum(joltage):
            raise Exception("No solution found")
        ans += r

    print("Part 2:", ans)

# What are the fewest buttons needed to solve this state
def dp2(state, buttons, mem):
    if state in mem:
        return mem[state]

    # check which buttons can be pressed without going negative
    possible_buttons = []
    for b in buttons:
        impossible = False
        for bi in b:
            if state[bi] <= 0:
                impossible = True
                break
        if not impossible:
            possible_buttons.append(b)

    # validate that every non-zero state has one possible button
    possible_states = {s for b in possible_buttons for s in b}
    for i, s in enumerate(state):
        if s > 0 and i not in possible_states:
            mem[state] = 9000
            return mem[state]

    # make sure to solve the smallest state first - since that is most likely lead to eliminations
    best = 9000
    smallest_index = sorted([(s, i) for i, s in enumerate(state) if s > 0])[0][1]
    for b in possible_buttons:
        if smallest_index not in b:
            continue
        new_state = list(state)
        for bi in b:
            new_state[bi] -= 1
        best = min(best, 1 + dp2(tuple(new_state), buttons, mem))

    mem[state] = best
    if len(mem) % 10000 == 0:
        print("Mem size:", len(mem), "Best solve:", max(sum(k) for k, v in mem.items() if v < 9000))
    return mem[state]

def main():
    TEST = "input/day10_test.txt"
    MAIN = "input/day10.txt"

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