import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day20{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    modules = parse(lines)
    flip_state, conj_state = setup_state(modules)
    ls, hs = 0, 0
    for i in range(1000):
        l, h, _ = send_pulses(modules, flip_state, conj_state)
        ls += l
        hs += h
    print(ls * hs)


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    modules = parse(lines)
    flip_state, conj_state = setup_state(modules)
    print("state size", len(flip_state), sum([len(v) for v in conj_state.values()]))

    # manual inspection
    # rg needs 4 high pulses
    # 4 upstreams are inverters - kd, zf, vg, gs
    check_for = ("kd", "zf", "vg", "gs")

    i = 0
    repeats = dict()
    while True:
        _, _, found = send_pulses(modules, flip_state, conj_state, check_for=check_for)
        i += 1
        for key in found:
            if key not in repeats:
                repeats[key] = i
                print("repeat:", i, key)
        if len(repeats) == len(check_for):
            mult = 1
            for v in repeats.values():
                mult = mult * v // gcd(mult, v)
            print("combined repeat", mult)
            break

    # TODO check, are there cycles ??
    # repeat: 3767
    # upstream is a conj with big fan-in and fan-out - tq, pf, kx, rj


def gcd(a, b):
    if a == 0:
        return b
    return gcd(b % a, a)


def parse(lines):
    modules = dict()
    for l in lines:
        t = "b"
        if l.startswith("%"):
            t = "%"
            l = l[1:]
        elif l.startswith("&"):
            t = "&"
            l = l[1:]
        key, targets = l.split(" -> ")
        modules[key] = (t, targets.split(", "))
    modules["output"] = ("o", [])

    return modules


def setup_state(modules):
    flip_state = dict()
    conj_state = defaultdict(lambda: dict())
    for key, (t, targets) in modules.items():
        if t == "%":
            flip_state[key] = False
        for dest in targets:
            if dest not in modules:
                print("Missing", dest)
                continue
            if modules[dest][0] == "&":
                conj_state[dest][key] = False
    return flip_state, conj_state


def send_pulses(modules, flip_state, conj_state, check_for=[]):
    lcount, hcount, check_found = 0, 0, set()

    q = Queue()
    q.put(("broadcaster", False, "button"))
    while not q.empty():
        key, high, src = q.get()
        # print(src, "high" if high else "low", key)
        if high:
            hcount += 1
        else:
            lcount += 1

        if key == "rx":
            if not high:
                rx_sent += 1
            continue

        t, targets = modules[key]
        if t == "b":
            for dest in targets:
                q.put((dest, high, key))
        elif t == "%":
            if not high:
                flip_state[key] = not flip_state[key]
                for dest in targets:
                    q.put((dest, flip_state[key], key))
        elif t == "&":
            conj_state[key][src] = high
            high_output = not all(conj_state[key].values())
            for dest in targets:
                q.put((dest, high_output, key))

            # looking for repeats
            if high_output and key in check_for:
                check_found.add(key)
        elif t == "o":
            pass
        else:
            raise Exception("Unknown type", t)

    return lcount, hcount, check_found




def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
