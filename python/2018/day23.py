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
    lines = util.parse_ints(filename)

    bots = []
    for l in lines:
        [x, y, z, r] = l
        bots.append(((x, y, z), r))

    xs = [b[0][0] for b in bots]
    xa = sum(xs) / len(bots)
    ya = sum([b[0][1] for b in bots]) / len(bots)
    za = sum([b[0][2] for b in bots]) / len(bots)

    # gradient descent
    best_p = (xa, ya, za)
    best_score = annealing_score(bots, best_p)

    iterations = 1000000
    max_dist = (max(xs) - min(xs)) / 10
    rounds = math.ceil(math.log2(max_dist))
    for i in range(iterations):
        if i % 100000 == 0:
            print(i)
        curr_round = round((i / iterations) * rounds)
        curr_dist = 2 ** (rounds - curr_round)

        (x, y, z) = best_p
        new_p = (x + r2()*curr_dist, y + r2()*curr_dist, z+r2()*curr_dist)
        new_score = annealing_score(bots, new_p)
        if new_score < best_score:
            print("new best", "point", new_p, "score", new_score, "dist", curr_dist, "iteration", i)
            best_score = new_score
            best_p = new_p

    # turns out none of the descents were needed
    print("in range", best_score)
    for p in [best_p]:
        print(p, dist((0, 0, 0), p))
        p2 = descent(bots, p, origin_closest_score)
        print("descent2", p2, dist((0, 0, 0), p2))

    # (36164950, 35655074, 49867108) 121687132 is too low. i.e. there are more than 898 bots
    # (38268620, 40447328, 46521286) 125237234 is too low (only 826 bots here, wtf)
    # 19648 980352 out / in range


# gravitate towards better points
def annealing_score(bots, point):
    score = 0
    c = range_count(bots, point)
    for bot in bots:
        d = dist(bot[0], point)
        until_range = d - bot[1]
        if until_range > 0:
            score += 1 / until_range
    return -c, dist((0,0,0), point), score


def r2():
    return random.random() * 2 - 1


# run across the edges of all range zone (diamond shaped)
def run_line(bots, p, r, sign1, sign2, index2):
    best_count = 0
    best_p = 0
    i = 0
    while i < r+1:
        pl = list(p)
        pl[2] += i * sign1
        pl[index2] += (r-i) * sign2
        pt = tuple(pl)

        count, skip = score_point(bots, pt)
        if count > best_count:
            best_count = count
            best_p = pt

        next_i = i + 1
        max_skip = min(r-1, i+skip)
        i = max(next_i, max_skip)

    return best_p, best_count


# magic number from previous bad solution
BOTS = 898


def score_point(bots, p):

    count = 0
    distances = []
    for b in bots:
        d = dist(b[0], p)
        distance_left = d - b[1]
        distances.append(distance_left)
        if distance_left <= 0:
            count += 1

    skip = 0
    if len(distances) > BOTS:
        # hack to make runtime not enormous
        # if we are below the known best solution, skip ahead until we are at a possible point
        distances = sorted(distances)
        if count < BOTS:
            skip = distances[BOTS-1]
        else:
            # TODO this might not be good - but cool if we add descent?
            skip = distances[count]

    return count, skip


def dist(p, p2):
    (x, y, z) = p
    (xb, yb, zb) = p2
    return abs(xb - x) + abs(yb - y) + abs(zb - z)


# gravitate towards the center, as long as range is the same
def origin_closest_score(bots, point):
    c = range_count(bots, point)
    return -c, dist((0, 0, 0), point)


def descent(bots, point, score_fn):
    p = point
    while True:
        best_n = p
        best_score = score_fn(bots, point)
        for point in nearby(point):
            s = score_fn(bots, point)
            if s < best_score:
                best_score = s
                best_n = p
        if best_n == p:
            break
        p = best_n
    return p


def nearby(point):
    [x, y, z] = point
    return [
        [x-1, y, z],
        [x+1, y, z],
        [x, y-1, z],
        [x, y+1, z],
        [x, y, z-1],
        [x, y, z+1]
    ]


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
