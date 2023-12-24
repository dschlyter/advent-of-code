import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day19{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    inst, parts = parse(lines)

    ans = 0
    for p in parts:
        res = process(inst, p)
        if res == "A":
            ans += sum(p.values())
    print(ans)


def parse(lines):
    break_index = lines.index("")

    inst = dict()
    for l in lines[:break_index]:
        l = l.replace("}", "")
        key, code_str = l.split("{")
        code_strs = code_str.split(",")
        inst[key] = list(map(lambda s: s.split(":"), code_strs))

    parts = []
    for l in lines[break_index + 1:]:
        part = dict()
        l = l.strip("{}")
        for kv in l.split(","):
            k, v = kv.split("=")
            part[k] = int(v)
        parts.append(part)

    return inst, parts


def process(inst, part):
    w = inst["in"]
    i = 0

    while i < len(w):
        rule = w[i]
        i += 1

        if len(rule) > 1:
            cond = rule[0]
            if "<" in cond:
                k, target = cond.split("<")
                if part[k] >= int(target):
                    continue
            if ">" in cond:
                k, target = cond.split(">")
                if part[k] <= int(target):
                    continue

        op = rule[-1]
        if op in ("A", "R"):
            return op
        w = inst[op]
        i = 0

    raise Exception("Unexpected termination")



def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    inst, parts = parse(lines)

    part = dict()
    for k in "xmas":
        part[k] = (1, 4001)

    ans = process_range(inst, part, inst["in"], 0)
    print(ans)


def process_range(inst, part, w, i):
    print(part, str(w[i]).ljust(20), i, w, sep="\t")

    # filter empty ranges
    for v in part.values():
        if v[0] >= v[1]:
            return 0

    rule = w[i]

    matching_part, unmatching_part = part.copy(), None
    if len(rule) > 1:
        unmatching_part = part.copy()
        cond = rule[0]
        if "<" in cond:
            k, target = cond.split("<")
            matching_part[k] = (part[k][0], int(target))
            unmatching_part[k] = (int(target), part[k][1])
        elif ">" in cond:
            k, target = cond.split(">")
            unmatching_part[k] = (part[k][0], int(target)+1)
            matching_part[k] = (int(target)+1, part[k][1])
        else:
            raise Exception("Unexpected condition")

    count = 0
    op = rule[-1]
    if op == "R":
        pass
    elif op == "A":
        combinations = 1
        for v in matching_part.values():
            combinations = combinations * (v[1] - v[0])
        count += combinations
        print(matching_part, "--> accept", combinations)
    else:
        count += process_range(inst, matching_part, inst[op], 0)

    if unmatching_part is not None:
        count += process_range(inst, unmatching_part, w, i + 1)

    return count

    # c 167409079868000
    # w 150959801000000

    # 88044910362976 too low


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
