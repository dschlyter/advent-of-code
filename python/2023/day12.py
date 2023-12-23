import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

suffix = sys.argv[1] if len(sys.argv) > 1 else ''
infile = f'input/day12{suffix}.txt'

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    for l in lines:
        wells, s2 = l.split(" ")
        counts = list(map(int, s2.split(",")))
        p = posibilities(wells, 0, False, [], counts, dict())
        print(wells, counts, p)
        ans += p
    print(ans)


def posibilities(line, i, prev_broken, counts, answer, mem):
    if i == len(line):
        if counts == answer:
            return 1
        return 0

    key = (i, prev_broken, tuple(counts), tuple(answer))
    if key in mem:
        return mem[key]

    # early check for abort
    for j in range(len(counts) - (1 if prev_broken else 0)):
        if j >= len(answer) or counts[j] != answer[j]:
            return 0

    pos = 0
    if line[i] in ('.', '?'):
        pos += posibilities(line, i + 1, False, counts, answer, mem)
    if line[i] in ('#', '?'):
        new_counts = counts.copy()
        if prev_broken:
            new_counts[-1] += 1
        else:
            new_counts.append(1)
        pos += posibilities(line, i + 1, True, new_counts, answer, mem)
    mem[key] = pos
    return pos


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    for l in lines:
        wells, s2 = l.split(" ")
        counts = list(map(int, s2.split(",")))
        wells = (wells + "?") * 4 + wells
        counts = counts * 5

        p = posibilities(wells, 0, False, [], counts, dict())
        print(wells, counts, p)
        ans += p
    print(ans)

    # time
    # without mem: 13.16
    # with mem: 0.035


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
