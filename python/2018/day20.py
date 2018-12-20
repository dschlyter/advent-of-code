import math
import sys

import glob
import pydash

from string import ascii_lowercase
from queue import Queue
import re
import sys

import util

from multiprocessing import Pool

input_name = 'input/day20'


def main():
    for file in glob.glob(f"{input_name}*"):
        print("Running file", file)
        problem1(file)
        problem2(file)

# 73425 too high


@util.timing
def problem1(filename):
    sys.setrecursionlimit(15000)

    lines = util.read_input(filename)
    l = lines[0][1:-1]
    graph = {}

    global visited_dfs
    visited_dfs = set()
    dfs(graph, l, 0, (0, 0))

    # util.print_states(graph.keys(), '.')

    prob1, prob2 = max_dist(graph)
    print(prob1)
    print(prob2)


def dfs(graph, line, index, pos):
    if index >= len(line):
        return

    # hacky opt for slowness
    global visited_dfs
    key = (pos, index)
    if key in visited_dfs:
        return
    visited_dfs |= {key}

    (x, y) = pos
    next_pos = None
    c = line[index]
    if c == 'N':
        next_pos = (x, y-1)
    elif c == 'S':
        next_pos = (x, y+1)
    if c == 'W':
        next_pos = (x-1, y)
    elif c == 'E':
        next_pos = (x+1, y)

    if next_pos:
        link(graph, pos, next_pos)
        dfs(graph, line, index+1, next_pos)
        return

    if c == '(':
        child_start = index+1
        while child_start < len(line):
            dfs(graph, line, child_start, pos)
            child_start = index_of(line, child_start, '|') + 1
    elif c == '|':
        new_index = index_of(line, index, ')') + 1
        dfs(graph, line, new_index, pos)
    elif c == ')':
        dfs(graph, line, index+1, pos)
    else:
        raise Exception(f"Invalid state {c}")


def link(graph, pos1, pos2):
    add_link(graph, pos1, pos2)
    add_link(graph, pos2, pos1)


def add_link(graph, pos1, pos2):
    if not graph.get(pos1):
        graph[pos1] = set()
    graph[pos1] |= {pos2}


def index_of(line, index, target, level=0):
    if index > len(line):
        return index

    c = line[index]
    new_level = level

    if level == 0 and c == target:
        return index
    if c == ')':
        if level <= 0:
            return len(line)
        else:
            new_level -= 1
    if c == '(':
        new_level += 1
    return index_of(line, index+1, target, new_level)


def max_dist(graph):
    max_dist, far_away = 0, 0

    q = Queue()
    visited = set()
    q.put(((0, 0), 0))

    while not q.empty():
        (pos, dist) = q.get()
        max_dist = max(max_dist, dist)
        if dist >= 1000:
            far_away += 1

        for p in graph[pos] - visited:
            visited |= {p}
            q.put((p, dist+1))

    return max_dist, far_away



@util.timing
def problem2(filename):
    lines = util.read_input(filename)



if __name__ == '__main__':
    main()
