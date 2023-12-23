import util
import copy
from collections import defaultdict

infile = 'input/day8.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    inst = lines[0]
    m = {}
    for l in lines[2:]:
        s, lr = l.split("=")
        l, r = lr.strip().replace("(", "").replace(")", "").split(", ")
        m[s.strip()] = (l, r)

    print(len(inst))
    count = 0
    pos = "AAA"
    while pos != "ZZZ":
        dir = inst[count % len(inst)]
        if dir == "L":
            pos = m[pos][0]
        elif dir == "R":
            pos = m[pos][1]
        else:
            raise Exception("Unknown direction")
        count += 1
    print(count, count / len(inst))


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    inst = lines[0]
    m = {}
    for l in lines[2:]:
        s, lr = l.split("=")
        l, r = lr.strip().replace("(", "").replace(")", "").split(", ")
        m[s.strip()] = (l, r)

    pos = list(filter(lambda k: k.endswith("A"), m.keys()))
    print("start positions", pos)

    schedules = []

    for p in pos:
        count = 0
        start, repeat = None, None
        z_node = None

        while True:
            i = count % len(inst)

            dir = inst[i]
            if dir == "L":
                p = m[p][0]
            elif dir == "R":
                p = m[p][1]
            else:
                raise Exception("Unknown direction")
            count += 1

            if p.endswith("Z"):
                if z_node is None:
                    z_node = (p, i)
                    start = count
                else:
                    if z_node != (p, i):
                        print(z_node, p, count)
                        raise Exception("sanity check: Multiple Z-nodes not supported")
                    repeat = count - start
                    if start != repeat:
                        raise Exception("sanity check: Not full loop - not supported")
                    schedules.append(repeat)
                    print("Schedule for", p, "is", start, repeat)
                    break

    merged = 1
    for repeat in schedules:
        merged = merged * repeat // gcd(merged, repeat)
    print(merged)


# GCD algorithm - thank you copilot
def gcd(a, b):
    if b == 0:
        return a
    return gcd(b, a % b)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
