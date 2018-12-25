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
input_name = 'input/day8'


def main():
    files = glob.glob(f"{input_name}*")
    for file in sorted(files, reverse=True):
        print("Running file", file)
        problem1(file)
        problem2(file)


@util.timing
def problem1(filename):
    data = util.parse_ints(filename)[0]

    root_node, index = read_node(data, 0)

    print(root_node.sum(), index, len(data))


def read_node(data, index):
    n = Node()
    n_childs = data[index]
    n_meta = data[index+1]
    index += 2

    for i in range(n_childs):
        node, index = read_node(data, index)
        n.child_nodes.append(node)

    for i in range(n_meta):
        n.metadata.append(data[index])
        index += 1

    return n, index


class Node:
    def __init__(self):
        self.child_nodes = []
        self.metadata = []
        self.saved_value = None

    def sum(self):
        return sum(self.metadata) + sum(map(lambda n: n.sum(), self.child_nodes))

    def value(self):
        if self.saved_value is not None:
            return self.saved_value

        if len(self.child_nodes) == 0:
            return sum(self.metadata)

        agg = 0
        for ci in self.metadata:
            if 0 < ci <= len(self.child_nodes):
                agg += self.child_nodes[ci - 1].value()
        self.saved_value = agg
        return agg


@util.timing
def problem2(filename):
    data = util.parse_ints(filename)[0]

    root_node, index = read_node(data, 0)

    print(root_node.value())


if __name__ == '__main__':
    main()
