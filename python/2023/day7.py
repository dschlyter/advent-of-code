import util
import copy
from collections import defaultdict

infile = 'input/day7.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    hands_sorted = sorted(lines, key=lambda x: hand_key(x.split(" ")[0]))

    ans = 0
    for i, hand in enumerate(hands_sorted):
        cards, bid = hand.split(" ")
        ans += int(bid) * (i + 1)
        print(i, bid, cards, hand_key(cards))
    print(ans)


def hand_key(hand):
    return [hand_type(hand)] + [card_order(c) for c in hand]


def hand_type(hand):
    counts = defaultdict(lambda: 0)
    for c in hand:
        counts[c] += 1
    s = sorted(counts.values())
    
    if s == [1, 1, 1, 1, 1]:
        return 1
    elif s == [1, 1, 1, 2]:
        return 2
    elif s == [1, 2, 2]:
        return 3
    elif s == [1, 1, 3]:
        return 4
    elif s == [2, 3]:
        return 5
    elif s == [1, 4]:
        return 6
    elif s == [5]:
        return 7
    else:
        print(s)
        raise Exception("Unknown hand type")


def card_order(c):
    return {
        'A': 14,
        'K': 13,
        'Q': 12,
        'J': 11,
        'T': 10,
    }.get(c) or int(c)


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]
    print(lines)

    hands_sorted = sorted(lines, key=lambda x: hand_key2(x.split(" ")[0]))

    ans = 0
    for i, hand in enumerate(hands_sorted):
        cards, bid = hand.split(" ")
        ans += int(bid) * (i + 1)
        print(i, bid, cards, hand_key2(cards))
    print(ans)


def hand_key2(hand):
    return [hand_type2(hand)] + [card_order2(c) for c in hand]


def hand_type2(hand):
    jokers = hand.count("J")
    if jokers == 5:
        return 7

    hand = hand.replace("J", "")
    counts = defaultdict(lambda: 0)
    for c in hand:
        counts[c] += 1
    s = sorted(counts.values())
    s[-1] = s[-1] + jokers
    
    if s == [1, 1, 1, 1, 1]:
        return 1
    elif s == [1, 1, 1, 2]:
        return 2
    elif s == [1, 2, 2]:
        return 3
    elif s == [1, 1, 3]:
        return 4
    elif s == [2, 3]:
        return 5
    elif s == [1, 4]:
        return 6
    elif s == [5]:
        return 7
    else:
        print(s)
        raise Exception("Unknown hand type")


def card_order2(c):
    return {
        'A': 14,
        'K': 13,
        'Q': 12,
        'J': 1,
        'T': 10,
    }.get(c) or int(c)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
