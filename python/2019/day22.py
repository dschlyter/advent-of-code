import math
import random
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue
from typing import List

import util

sys.setrecursionlimit(15000)


def part1(lines: List[str]):
    ans = None

    card_count = 10007

    if len(lines) < 20:
        card_count = 10

    cards = list(range(0, card_count))
    hist = []

    for line in lines:
        for i, card in enumerate(cards):
            if card == 2019:
                hist.append(i)
                break

        if line == "deal into new stack":
            cards = list(reversed(cards))
        elif line.startswith("cut "):
            N = int(line.split(" ")[1])
            if N < 0:
                N = len(cards) + N
            cards = cards[N:] + cards[:N]
        elif line.startswith("deal with increment "):
            N = int(line.split(" ")[-1])
            table = [-1] * len(cards)
            i = 0
            for card in cards:
                if table[i] != -1:
                    raise Exception("Failed deal "+str(table))
                table[i] = card
                i = (i + N) % len(table)
            cards = table
        else:
            raise Exception("unmatched "+line)

    if len(cards) < 1000:
        print(cards)

    for i, card in enumerate(cards):
        if card == 2019:
            ans = i

    print(list(reversed(hist)))
    print("part 1 answer", ans)


def part2(lines: List[str]):
    ans = None

    # cards = 119315717514047
    # times = 101741582076661
    # pos = 2020
    cards = 10007
    pos = 5472

    lines = list(reversed(lines))
    ops = []

    for line in lines:
        if line == "deal into new stack":
            ops.append(0)
        elif line.startswith("cut "):
            N = int(line.split(" ")[1])
            if N < 0:
                N = cards + N
            assert(N > 0)
            ops.append(-N)
        elif line.startswith("deal with increment "):
            N = int(line.split(" ")[-1])
            inv = modinv(N, cards)
            assert(inv > 0)
            ops.append(inv)

    visited = set()
    count = 0
    last_pos = 0

    while pos not in visited:
        count += 1
        visited.add(pos)
        # if count % 100000 == 0:
        print(count, pos, pos-last_pos)
        last_pos = pos

        for op in ops:
            if op == 0:
                pos = cards - 1 - pos
            elif op < 0:
                N = -op
                if pos > cards - N:
                    pos -= (cards - N)
                else:
                    pos += N
            elif op > 0:
                N = op
                pos = (pos * N) % cards

    print(count)

    print("part 2 answer", pos)


# stolen from: https://stackoverflow.com/questions/4798654/modular-multiplicative-inverse-function-in-python
def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)


def modinv(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m


if __name__ == '__main__':
    util.main(__file__, part1, part2, match="day22.txt")
