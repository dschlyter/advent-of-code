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

    cards = 119315717514047
    pos = 2020
    times = 101741582076661

    # cards = 10007; pos = 5472; times = 1

    lines = list(reversed(lines))

    k = 1
    m = 0

    for line in lines:
        if line == "deal into new stack":
            k *= -1
            m *= -1
            m = (m - 1) % cards
        elif line.startswith("cut "):
            N = int(line.split(" ")[1])
            if N < 0:
                N = cards + N
            assert(N > 0)

            m = (m + N) % cards
        elif line.startswith("deal with increment "):
            N = int(line.split(" ")[-1])
            inv = modinv(N, cards)
            assert(inv > 0)

            k = (k * inv) % cards
            m = (m * inv) % cards

    powers = [(k, m)]
    for i in range(1, 50):
        k0, m0 = powers[i-1]
        k = (k0 * k0) % cards
        m = (k0 * m0 + m0) % cards
        powers.append((k, m))

    for i in range(0, 50):
        powpow = 2 ** i
        if times & powpow > 0:
            k, m = powers[i]
            pos = (k * pos + m) % cards

    # 19219759037745 is too low :(
    # 46458976142918 is too low :(
    # 101842973938400 is too high :(

    """
    8150301316572
    74518628734685
    41566245312604
    51529718527781
    84809471150302
    34955862758706
    54907196857247
    77653475936411
    108150807952582
    24095646560451
    """

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
