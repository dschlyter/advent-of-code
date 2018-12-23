import math
import random
import sys

import glob
import time
from datetime import datetime

import pydash

from string import ascii_lowercase
from queue import Queue, PriorityQueue
import re
import sys

import util

from multiprocessing import Pool
from simanneal import Annealer

sys.setrecursionlimit(15000)
input_name = 'input/day23.txt'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    lines = util.parse_ints(filename)

    best_range = 0
    best_bot = 0

    for l in lines:
        r = l[3]
        if r > best_range:
            best_range = r
            best_bot = l

    count = 0
    for bot in lines:
        if dist1(bot, best_bot) <= best_range:
            count += 1

    print(count)


def dist1(bot, other_bot):
    [x, y, z, r] = bot
    [xb, yb, zb, rb] = other_bot
    return abs(xb - x) + abs(yb - y) + abs(zb - z)


@util.timing
def problem2(filename):
    print("You should run this with pypy, or be prepared to wait hours")
    lines = util.parse_ints(filename)

    bots = []
    for l in lines:
        [x, y, z, r] = l
        bots.append(((x, y, z), r))

    # begin with the best bot position
    # using center and edge positions
    best_score = sys.maxsize
    best_found = None
    for b in bots:
        for x in [-1, 0, 1]:
            for y in [-1, 0, 1]:
                for z in [-1, 0, 1]:
                    (xc, yc, zc) = b[0]
                    r = b[1]
                    p = (xc+x*r, yc+y*r, zc+z*r)
                    score = annealing_score(bots, p)
                    if score < best_score:
                        best_score = score
                        best_found = p

    # TODO you need to enable this to get the right answer (you can also tune down time)
    # output from previous algo (see git history)
    # best_found = (38087687, 35655074, 51789845)

    print(best_found)
    ra = RangeAnnealer(bots, best_found)
    run_time_minutes = 5
    schedule = ra.auto(run_time_minutes)
    print("found schedule", schedule)
    ra.set_schedule(schedule)
    point, score = ra.anneal()

    print("answer", point, score, range_count(bots, point), dist((0, 0, 0), point))

    # (38087689, 35655078, 51789840) 125532606 with 942 bots is the right answer (from anealing)


class RangeAnnealer(Annealer):
    def __init__(self, bots, p):
        self.bots = bots
        self.state = p

    def move(self):
        l = list(self.state)
        index = random.randint(0, 2)
        l[index] += random.choice([-1, 1])
        self.state = tuple(l)

    def energy(self):
        return annealing_score(self.bots, self.state)


# gravitate towards better points
def annealing_score(bots, point):
    score = 0
    c = range_count(bots, point)
    for bot in bots:
        d = dist(bot[0], point)
        until_range = d - bot[1]
        if until_range > 0:
            score += 1 / until_range
    return -c * 10000000 + max(0, dist((0,0,0), point)) + score * 0.01


def r2():
    return random.random() * 2 - 1


def dist(p, p2):
    (x, y, z) = p
    (xb, yb, zb) = p2
    return abs(xb - x) + abs(yb - y) + abs(zb - z)


def range_count(bots, point):
    count = 0
    for bot in bots:
        if in_range(point, bot):
            count += 1
    return count


def in_range(point, bot):
    return dist(bot[0], point) <= bot[1]


def smaller(x):
    if x > 0:
        return x-1
    elif x < 0:
        return x+1
    else:
        return 0


if __name__ == '__main__':
    main()
