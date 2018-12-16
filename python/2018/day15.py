import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day15.txt'
attack_power = 3


def main():
    fns = ["day15_test",
           "day15_test1",
           "day15_test2",
           "day15_test3",
           "day15_test4",
           "day15_test5",
           "day15",
           ]
    # fns = [fns[0]]
    for f in fns:
        print(f)
        problem1(f)
    for f in fns:
        print(f)
        problem2(f)


@util.timing
def problem1(file):
    lines = util.read_input("input/"+file+".txt")
    terrain, units = parse_input(lines)

    ticks = 0
    for i in range(10000):
        end = False

        player_order = pydash.sort_by(units, POS)
        for p in player_order:
            if p not in units:
                continue

            if len(set(map(lambda u: u[TEAM], units))) < 2:
                end = True
                break

            move(p, terrain, units, 3)

        if end:
            break

        ticks += 1

        # print(ticks)
        # print_state(terrain, units)

    print(ticks)
    print_state(terrain, units)

    health_sum = sum(map(lambda u: u[HEALTH], units))
    print(ticks * health_sum)


@util.timing
def problem2(file):

    # interestingly, the best search method is brute force
    best, attack = brute_force(file)  # 10 seconds
    # best, attack = binary_search(file, lambda l, u: (l+u) // 2)  # 13 seconds
    # best, attack = binary_search(file, lambda l, u: (4*l+u) // 5)  # 10 seconds
    # best, attack = binary_search(file, lambda l, u: l + math.floor(math.sqrt(u-l) - 0.1))  # 10 seconds
    # best, attack = incrementing_binary_search(file)  # 12 seconds

    (terrain, units, ticks) = best
    print(ticks)
    print_state(terrain, units)

    print("attack", attack)
    health_sum = sum(map(lambda u: u[HEALTH], units))
    print(ticks * health_sum)


def brute_force(file):
    for attack in range(3, 201):
        res = no_loss_battle(file, attack)

        if res:
            return res, attack


def binary_search(file, next_test):
    l, u = 3, 301
    best_t = 301
    best = None

    # brute force
    # binary search, but interestingly
    while l < u:
        t = next_test(l, u)

        res = no_loss_battle(file, t)

        if not res:
            l = t+1
        else:
            u = t
            if t < best_t:
                best_t = t
                best = res

    return best, best_t


def incrementing_binary_search(file):
    l, u = 3, None
    best_t = 301
    best = None

    while not u or l < u:
        # double upper bound until it is established
        if u is None:
            t = (l-1) * 2
        else:
            t = (l + u) // 2

        res = no_loss_battle(file, t)

        if not res:
            l = t+1
        else:
            u = t
            if t < best_t:
                best_t = t
                best = res

    return best, best_t


def no_loss_battle(file, attack_power):
    print("testing attack", attack_power)
    lines = util.read_input("input/"+file+".txt")
    terrain, units = parse_input(lines)

    elves = len(list(filter(lambda u: u[TEAM] == 'E', units)))

    ticks = 0
    for i in range(10000):
        end = False

        player_order = pydash.sort_by(units, POS)
        for p in player_order:
            if p not in units:
                continue


            if len(set(map(lambda u: u[TEAM], units))) < 2:
                end = True
                break

            ap = attack_power if p[TEAM] == 'E' else 3
            move(p, terrain, units, ap)

        alive = len(list(filter(lambda u: u[TEAM] == 'E', units)))
        if alive < elves:
            return False

        if end:
            break

        ticks += 1

        # print(ticks)
        # print_state(terrain, units)

    return terrain, units, ticks


def move(p, terrain, units, attack_power):
    um = {}
    for u in units:
        um[u[POS]] = u

    next_pos = bfs(p, terrain, um)
    if next_pos and not um.get(next_pos):
        p[POS] = next_pos

    adj = set(nearby(p[POS]))
    attackable = pydash.chain(units) \
        .filter(lambda ou: (ou[POS] in adj) and (ou[TEAM] != p[TEAM])) \
        .sort_by(lambda u: (u[HEALTH], u[POS])) \
        .value()

    if attackable:
        target = attackable[0]
        target[HEALTH] -= attack_power
        if target[HEALTH] <= 0:
            units.remove(target)


def bfs(player, terrain, unit_map):
    q = Queue()
    visited = set()
    solutions = []
    best_dist = 9001

    for p in nearby(player[POS]):
        q.put((1, p, p))

    while not q.empty():
        item = q.get()
        (dist, pos, move) = item
        if dist > best_dist:
            continue
        (y, x) = pos
        if terrain[y][x] != ".":
            continue
        if pos in visited:
            continue
        visited |= {pos}

        u = unit_map.get(pos)
        if u:
            if u[TEAM] != player[TEAM]:
                solutions.append(item)
                best_dist = dist
            continue

        for np in nearby(pos):
            q.put((dist+1, np, move))

    if solutions:
        return sorted(solutions)[0][2]

    return None


def nearby(pos):
    (y, x) = pos
    # returned in read order
    return [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]


def parse_input(lines):
    terrain = []
    units = []

    y = 0
    for line in lines:
        terrain.append([])
        x = 0
        for t in line:
            if t == 'E' or t == 'G':
                units.append({HEALTH: 200, POS: (y, x), TEAM: t, ID: len(units)})
                t = "."
            terrain[-1].append(t)
            x += 1
        y += 1

    return terrain, units


def print_state(terrain, units):
    p = []
    stats = {}
    for t in terrain:
        p.append(t.copy())
    for u in units:
        (y, x) = u[POS]
        p[y][x] = u[TEAM]
        stats[y] = stats.get(y, [])
        stats[y].append(u)
    y = 0
    for t in p:
        print("".join(t), end=" ")
        for p in pydash.sort_by(stats.get(y, []), POS):
            print(f"{p[TEAM]}({p[HEALTH]})", end=" ")
        print()
        y += 1


HEALTH = 'health'
POS = 'pos'
TEAM = 'team'
ID = 'id'


if __name__ == '__main__':
    main()
