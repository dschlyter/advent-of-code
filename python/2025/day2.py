import util
import sys

from util import get

def main():
    lines = util.read_input(get(sys.argv, 1, "input/day2.txt"))
    p1(lines)
    p2(lines)

def p1(lines):
    ans = 0
    for p in lines[0].split(","):
        a, b = p.split("-")
        for i in range(int(a), int(b) + 1):
            s = str(i)
            if len(s) % 2 == 0 and s[0:len(s)//2] == s[len(s)//2:]:
                ans += i
    
    print("Part 1:", ans)

def p2(lines):
    ans = 0
    for p in lines[0].split(","):
        a, b = p.split("-")
        for i in range(int(a), int(b) + 1):
            s = str(i)
            sl = len(s)
            invalid = False
            for l in range(1, sl // 2 + 1):
                if sl % l != 0:
                    continue

                not_invalid = False
                for o in range(0, sl, l):
                    if s[o:o+l] != s[0:l]:
                        not_invalid = True
                        break

                if not not_invalid:
                    invalid = True
                    break

            if invalid:
                ans += i

    print("Part 2:", ans)

if __name__ == "__main__":
    main()