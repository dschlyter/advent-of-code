import util
import sys

from util import get

def main():
    lines = util.read_input(get(sys.argv, 1, "input/day3.txt"))
    p1(lines)
    p2(lines)

def p1(lines):
    ans = 0
    for line in lines:
        l_max = 0
        for i in range(len(line)):
            for j in range(i+1, len(line)):
                l_max = max(l_max, int(line[i]) * 10 + int(line[j]))
        ans += l_max
    print("Part 1:", ans)

def p2(lines):
    ans = 0
    for line in lines:
        mem = {}
        a = dp(mem, 12, len(line) - 1, line)
        ans += a
    print("Part 2:", ans)

def dp(mem, batteries, pos, line):
    key = (batteries, pos)
    if key in mem:
        return mem[key]

    ans = 0
    if batteries > 0 and pos >= 0:
        ans = max(ans, dp(mem, batteries - 1, pos - 1, line) * 10 + int(line[pos]))
    if pos >= 1:
        ans = max(ans, dp(mem, batteries, pos - 1, line))

    mem[key] = ans
    return mem[key]
    

if __name__ == "__main__":
    main()

811111111119
11111111111119