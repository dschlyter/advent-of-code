import util
import copy
from collections import defaultdict

infile = 'input/day9.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    for l in lines:
        n = list(map(int, l.split(" ")))

        diffs = [n]
        while set(diffs[-1]) != {0}:
            diffs.append(differences(diffs[-1]))

        for i in range(0, len(diffs)-1):
            j = len(diffs) - 2 - i
            diffs[j].append(diffs[j][-1] + diffs[j + 1][-1])

        for d in diffs:
            print(d)
        ans += diffs[0][-1]

    print(ans)


def differences(n):
    ans = []
    for i in range(1, len(n)):
        ans.append(n[i] - n[i - 1])
    return ans


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    for l in lines:
        n = list(map(int, l.split(" ")))

        diffs = [n]
        while set(diffs[-1]) != {0}:
            diffs.append(differences(diffs[-1]))
        for d in diffs:
            print(d)

        for i in range(0, len(diffs)-1):
            j = len(diffs) - 2 - i
            diffs[j].insert(0, diffs[j][0] - diffs[j + 1][0])

        for d in diffs:
            print(d)
        ans += diffs[0][0]

    print(ans)

# GCD algorithm - thank you copilot
def gcd(a, b):
    if b == 0:
        return a
    return gcd(b, a % b)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
