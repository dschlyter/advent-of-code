import util
from collections import defaultdict

infile = 'input/day4.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    for l in lines:
        content = l.split(":")[1]
        winning, having = content.split("|")
        wins = {int(n) for n in winning.strip().split(" ") if n != ""}
        mine = {int(n) for n in having.strip().split(" ") if n != ""}
        match = wins.intersection(mine)
        if match:
            points = 2 ** (len(match) - 1)
            ans += points

    print(ans)


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    copies = [1] * len(lines)
    for i, l in enumerate(lines):
        content = l.split(":")[1]
        winning, having = content.split("|")
        wins = {int(n) for n in winning.strip().split(" ") if n != ""}
        mine = {int(n) for n in having.strip().split(" ") if n != ""}
        match = wins.intersection(mine)
        for j in range(len(match)):
            copies[i+j+1] += copies[i]
        print(i, match, copies[i])

    print(sum(copies))


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
