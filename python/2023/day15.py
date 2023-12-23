import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day15{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    ans = 0
    for step in lines[0].split(","):
        ans += hash(step)
    print(ans)



def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]
    
    hashmap = defaultdict(lambda: [])
    for step in lines[0].split(","):
        if "-" in step:
            key = step[:-1]
            box = hash(key)
            hashmap[box] = list(filter(lambda x: x[0] != key, hashmap[box]))
        elif "=" in step:
            key, val = step.split("=")
            box = hash(key)
            added = False
            for i, (k, _) in enumerate(hashmap[box]):
                if k == key:
                    hashmap[box][i] = (key, int(val))
                    added = True
            if not added:
                hashmap[box].append((key, int(val)))
        else:
            raise Exception("Unknown step")

    ans = 0 
    for box_i, items in hashmap.items():
        for i, (k, v) in enumerate(items):
            f = (box_i + 1) * (i + 1) * v
            ans += f
    print(ans)


def hash(line):
    val = 0
    for c in line:
        # ascii code of c
        val = val + ord(c)
        val *= 17
        val = val % 256
    return val


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
