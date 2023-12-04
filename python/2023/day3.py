import util
from collections import defaultdict

infile = 'input/day3.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    nums = list(map(str, range(10)))
    ans = 0

    for y, l in enumerate(lines):
        x = 0
        num = 0
        is_part = False
        while x < len(l):
            c = l[x]
            if c in nums:
                num = num * 10 + int(c)
                for yd in range(-1, 2):
                    for xd in range(-1, 2):
                        ac = get(get(lines, y+yd), x+xd)
                        if ac is not None and ac not in nums and ac != ".":
                            is_part = True
            else:
                if is_part:
                    ans += num
                num = 0
                is_part = False
            x += 1
        if is_part:
            ans += num

    print(ans)

            


def problem2():
    with open(infile) as fp:
        lines = [l.strip() + "." for l in fp]

    nums = list(map(str, range(10)))

    gears = defaultdict(list)

    for y, l in enumerate(lines):
        x = 0
        adjacent_gears = set()
        num = 0
        while x < len(l):
            c = l[x]
            if c in nums:
                num = num * 10 + int(c)
                for yd in range(-1, 2):
                    for xd in range(-1, 2):
                        ac = get(get(lines, y+yd), x+xd)
                        if ac == "*":
                            adjacent_gears.add((x+xd, y+yd))
            else:
                if num > 0:
                    for g in adjacent_gears:
                        gears[g].append(num)
                adjacent_gears = set()
                num = 0
            x += 1

    ans = 0
    for g in gears.values():
        if len(g) != 2:
            print("Ignoring gear", g, "because it has", len(g), "values")
            continue
        print(g)
        ans += g[0] * g[1]
    print(ans)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
