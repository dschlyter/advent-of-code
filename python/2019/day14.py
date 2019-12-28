import math
import sys
from collections import defaultdict
from queue import Queue
from typing import List

import util

sys.setrecursionlimit(15000)


def part1(lines: List[str]):
    inputs = {}
    target_output = {}
    for line in lines:
        split = line.split("=>")
        output_amount, target = split[1].strip().split(" ")
        inp_list = []
        for inp in split[0].split(","):
            input_amount, source = inp.strip().split(" ")
            inp_list.append((source, int(input_amount)))

        inputs[target] = inp_list
        target_output[target] = int(output_amount)

    order = {key: 0 for key in inputs.keys()}
    while True:
        change = False
        for key in order.keys():
            for inp in inputs[key]:
                inp_key = inp[0]
                if inp_key == 'ORE':
                    continue
                if order[inp_key] <= order[key]:
                    order[inp_key] = order[key] + 1
                    change = True
        if not change:
            break

    inventory = {'FUEL': 1}

    for _, chemical in sorted([(value, key) for key, value in order.items()]):
        if chemical in inventory:
            reactions = math.ceil(inventory[chemical] / target_output[chemical])
            del inventory[chemical]
            for source, input_amount in inputs[chemical]:
                inventory[source] = inventory.get(source, 0) + input_amount * reactions

    print("part 1 answer", inventory)


def part2(lines: List[str]):
    inputs = {}
    target_output = {}
    for line in lines:
        split = line.split("=>")
        output_amount, target = split[1].strip().split(" ")
        inp_list = []
        for inp in split[0].split(","):
            input_amount, source = inp.strip().split(" ")
            inp_list.append((source, int(input_amount)))

        inputs[target] = inp_list
        target_output[target] = int(output_amount)

    order = {key: 0 for key in inputs.keys()}
    while True:
        change = False
        for key in order.keys():
            for inp in inputs[key]:
                inp_key = inp[0]
                if inp_key == 'ORE':
                    continue
                if order[inp_key] <= order[key]:
                    order[inp_key] = order[key] + 1
                    change = True
        if not change:
            break

    for fuel in range(2700000, 3300000):
        if fuel % 10000 == 0:
            print("progress", fuel)

        inventory = {'FUEL': fuel}

        for _, chemical in sorted([(value, key) for key, value in order.items()]):
            if chemical in inventory:
                reactions = math.ceil(inventory[chemical] / target_output[chemical])
                del inventory[chemical]
                for source, input_amount in inputs[chemical]:
                    inventory[source] = inventory.get(source, 0) + input_amount * reactions

        if inventory['ORE'] > 1_000_000_000_000:
            print("part 2 answer", fuel-1)
            break

    print("part 2 answer", inventory)


if __name__ == '__main__':
    util.main(__file__, part1, part2)
