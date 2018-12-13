import math
import sys

import pydash

from string import ascii_lowercase
from queue import Queue
import re

import util

from multiprocessing import Pool

filename = 'input/day13.txt'


def main():
    problem1()
    problem2()


@util.timing
def problem1():
    lines = util.read_input(filename)
    track, carts = parse(lines)
    show_state(track, carts)

    for cnt in range(1000):
        print(cnt)
        carts = sorted(carts)
        for i in range(len(carts)):
            c = carts[i]
            (pos, d, count) = c
            (y, x) = pos

            part = track[y][x]
            if part == "/":
                if d == up:
                    d = right
                elif d == right:
                    d = up
                elif d == down:
                    d = left
                elif d == left:
                    d = down
            elif part == "\\":
                if d == up:
                    d = left
                elif d == left:
                    d = up
                elif d == down:
                    d = right
                elif d == right:
                    d = down
            elif part == "+":
                if count != 1:
                    d = turn(d, count+1)
                count = (count + 1) % 3

            (yd, xd) = d
            new_pos = (y + yd, x + xd)
            carts[i] = (new_pos, (yd, xd), count)

            for j in range(len(carts)):
                oc = carts[j]
                if i != j and c[0] == oc[0]:
                    print("Collision", c[0][1], c[0][0])
                    return
        # show_state(track, carts)


up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)
dirs = [up, left, down, right]


def turn(direction, index):
    i = dirs.index(direction)
    return dirs[(len(dirs) + i + index) % len(dirs)]


def show_state(track, carts):
    cart_post = set(map(lambda x: x[0], carts))
    for y in range(len(track)):
        l = track[y]
        for x in range(len(l)):
            if (y, x) in cart_post:
                print('X', end="")
            else:
                print(l[x], end="")
        print("")


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
