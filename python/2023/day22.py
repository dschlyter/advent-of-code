import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue, PriorityQueue

suffix = sys.argv[1] if len(sys.argv) > 1 else ""
infile = f"input/day22{suffix}.txt"

sys.setrecursionlimit(20_000)


def main():
    # problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    bricks = parse(lines)
    drop_bricks(bricks)
            
    vaskable = 0
    occupied = set()
    for brick in bricks:
        occupied.update(to_set(brick))
    for i, brick in enumerate(bricks):
        can_be_vasked = True
        bs = to_set(brick)
        occupied.difference_update(bs)
        for j, other_brick in enumerate(bricks):
            if i == j:
                continue
            occupied.difference_update(to_set(other_brick))
            obf = fall(other_brick)
            if height(obf) > 0 and not occupied.intersection(to_set(obf)):
                can_be_vasked = False
                print("Brick", i, "is only support of brick", j)
            occupied.update(to_set(other_brick))
        occupied.update(bs)
        if can_be_vasked:
            vaskable += 1
    print("Vaskd", vaskable)


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    bricks = parse(lines)
    drop_bricks(bricks)

    ans = 0
    for i, brick in enumerate(bricks):
        occupied = set()
        for b in bricks:
            occupied.update(to_set(b))

        # remove this brick
        occupied.difference_update(to_set(brick))
        gone = set([i])

        # slow scan of all bricks
        while True:
            fallen = False

            for j, b in enumerate(bricks):
                if j in gone:
                    continue
                occupied.difference_update(to_set(b))
                if height(b) > 1 and not occupied.intersection(to_set(fall(b))):
                    fallen = True
                    gone.add(j)
                else:
                    occupied.update(to_set(b))

            if not fallen:
                break
        
        if len(gone) > 1:
            ans += len(gone) - 1
        print("Checking brick", i, len(gone) - 1, ans)
    print("Fallen bricks", ans)


def parse(lines):
    bricks = []
    for l in lines:
        s = l.split("~")
        bricks.append(list(map(lambda x: list(map(int, x.split(","))), s)))

    return bricks


def drop_bricks(bricks):
    # drop the bricks
    for step in range(1000):
        print("Droppin", step)
        fallen = False

        occupied = set()
        for brick in bricks:
            occupied.update(to_set(brick))

        for i, brick in enumerate(bricks):
            bs = to_set(brick)
            occupied.difference_update(bs)

            bf = fall(brick)
            bfs = to_set(bf)
            if not occupied.intersection(bfs) and height(bf) > 0:
                bricks[i] = bf
                occupied.update(bfs)
                fallen = True
            else:
                occupied.update(bs)

        if not fallen:
            break


def to_set(brick):
    s = set()
    for x in range(brick[0][0], brick[1][0] + 1):
        for y in range(brick[0][1], brick[1][1] + 1):
            for z in range(brick[0][2], brick[1][2] + 1):
                s.add((x, y, z))
    return s


def height(brick):
    return min([z for x, y, z in brick])


def fall(brick, offset=1):
    b2 = copy.deepcopy(brick)
    b2[0][2] -= offset
    b2[1][2] -= offset
    return b2


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]


if __name__ == "__main__":
    main()
