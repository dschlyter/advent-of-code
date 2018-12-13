import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day13_test.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    lines = util.read_input(filename)
    track, carts = parse(lines)
    show_state(track, carts)

    cnt = 0
    while True:
        cnt += 1
        print(cnt)
        carts = sorted(carts)
        for i in range(len(carts)):
            c = carts[i]
            (pos, direction, count) = c
            (y, x) = pos
            (yd, xd) = direction

            part = track[y][x]

            if part ==

            new_pos = (y + yd, x + xd)
            carts[i] = (new_pos, (yd, xd), count)

            for j in range(len(carts)):
                oc = carts[j]
                if i != j and c[0] == oc[0]:
                    print("Collision", c[0])
                    return



def show_state(track, carts):
    for l in track:
        print(l)
    print(carts)


up = (-1, 0)
down = (1, 0)
left = (-1, 0)
right = (1, 0)
dirs = [up, down, left, right]


def parse(lines):
    track = []
    carts = []

    y = 0
    for l in lines:
        x = 0
        line = ""
        for p in l:
            if p == "<":
                carts.append(((y, x), left, 0))
                line += "-"
            elif p == ">":
                carts.append(((y, x), right, 0))
                line += "-"
            elif p == "^":
                carts.append(((y, x), up, 0))
                line += "|"
            elif p == "v":
                carts.append(((y, x), down, 0))
                line += "|"
            else:
                line += p
            x += 1
        track.append(line)
        y += 1

    return track, carts


@util.timing
def problem2():
    pass


if __name__ == '__main__':
    main()
