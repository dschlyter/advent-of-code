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

from pprint import pprint

sys.setrecursionlimit(15000)
input_name = 'input/day24'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    lines = util.read_input(filename)
    armies = parse(lines)
    for army in armies:
        pprint(list(map(lambda x: x.pprint(), army)))
        pass
    remaining = fight(armies[0] + armies[1])
    s = sum(map(lambda a: a.size, remaining))
    print(s)

    # 18528 is too low


def fight(armies):
    display(armies)

    while len(set(map(lambda x: x.side, armies))) > 1:
        # target selection
        armies = sorted(armies, key=lambda a: a.priority(), reverse=True)
        targets = {}

        for army in armies:
            best_prio = 0, 0, 0
            best_target_id = None
            for other in armies:
                if other.side == army.side:
                    continue
                if other.id in targets.values():
                    continue
                d = other.damage_from(army)
                p = other.priority()
                key = d, p[0], p[1]
                if d > 0 and key > best_prio:
                    best_prio = key
                    best_target_id = other.id
            if best_target_id:
                # print(army.id, "select", best_target_id)
                targets[army.id] = best_target_id

        # attack
        armies = sorted(armies, key=lambda a: a.initiative, reverse=True)

        for army in armies:
            target_id = targets.get(army.id)
            if target_id is None:
                continue
            if army.size <= 0:
                continue
            target = next(filter(lambda a: a.id == target_id, armies))
            d = target.damage_from(army)
            loss = d // target.hp
            target.size -= loss
            # print(army.id, "attacks", target.id, "killing", loss)

        armies = list(filter(lambda a: a.size > 0, armies))
        # display(armies)

    return armies


def display(armies):
    res = list(map(lambda a: f"{a.id} {a.side} {a.size}*{a.damage} ({a.hp})", armies))
    print(" - ".join(res))


def parse(lines):
    si = lines.index("")
    army1 = parse_army(lines[:si], 1)
    army2 = parse_army(lines[si+1:], 2)
    return [army1, army2]


index = 0


def parse_army(lines, side):
    global index
    lines = lines[1:]
    ret = []

    for l in lines:
        index += 1
        i = util.parse_int_line(l)
        size = i[0]
        hp = i[1]
        damage = i[2]
        initiative = i[3]

        words = l.split(" ")
        damage_index = words.index("damage") - 1
        damage_type = words[damage_index]

        immune = []
        weakness = []
        if "(" in l:
            l2 = l[l.index("(")+1:l.index(")")]
            s = l2.split(";")
            for sp in s:
                sp = sp.strip()
                w = sp.replace(",", "").split(" ")[2:]
                if sp.startswith("weak"):
                    weakness += w
                elif sp.startswith("immune"):
                    immune += w
                else:
                    raise Exception("unable to parse "+sp)

        ret.append(Army(index, side, size, hp, damage, damage_type, initiative, immune, weakness))

    return ret


class Army:
    def __init__(self, id, side, size, hp, damage, damage_type, initiative, immune, weakness):
        self.id = id
        self.side = side
        self.size = size
        self.hp = hp
        self.damage = damage
        self.damage_type = damage_type
        self.initiative = initiative
        self.immune = immune
        self.weakness = weakness

    def priority(self):
        return self.power(), self.initiative

    def power(self):
        return self.size * self.damage

    def damage_from(self, other):
        if other.damage_type in self.immune:
            return 0

        dmg = other.power()

        if other.damage_type in self.weakness:
            dmg *= 2

        return dmg

    def pprint(self):
        return self.__dict__


@util.timing
def problem2(filename):
    lines = util.read_input(filename)
    armies = parse(lines)
    for army in armies:
        pprint(list(map(lambda x: x.pprint(), army)))
        pass
    remaining = fight(armies[0] + armies[1])
    s = sum(map(lambda a: a.size, remaining))
    print(s)


if __name__ == '__main__':
    main()
