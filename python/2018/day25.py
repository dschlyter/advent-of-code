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
input_name = 'input/day25'


def main():
    files = glob.glob(f"{input_name}*")
    for file in sorted(files, reverse=True):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    points = util.parse_ints(filename)

    umap = {}
    for i in range(len(points)):
        for j in range(i+1, len(points)):
            if dist(points[i], points[j]) <= 3:
                union(umap, i, j)

    constelations = set()
    for i in range(len(points)):
        group = find(umap, i)
        constelations.add(group)

    print(len(constelations), constelations)
    # 0, 1, 3, 6, 7
    # 7-8
    # 2-9


def union(umap, i1, i2):
    iv = find(umap, i1)
    i2v = find(umap, i2)
    value = min(iv, i2v)
    umap[iv] = value
    umap[i2v] = value


def find(umap, i):
    group = umap.get(i)
    if group is None or group == i:
        return i
    f = find(umap, group)
    if f != group:
        umap[i] = f
    return f


def dist(pos1, pos2):
    dist = 0
    for i in range(len(pos1)):
        dist += abs(pos1[i] - pos2[i])
    return dist


@util.timing
def problem2(filename):
    pass


if __name__ == '__main__':
    main()
