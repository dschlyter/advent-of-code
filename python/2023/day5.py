import util
import copy
from collections import defaultdict

infile = 'input/day5.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    targets = list(map(int, lines[0].split(" ")[1:]))
    new_targets = targets.copy()
    maps = lines[2:]

    for l in maps:
        if l == "":
            targets = new_targets
            new_targets = targets.copy()
            continue
        if ":" in l:
            print("processing", l)
            continue
        dest, src, size = list(map(int, l.split(" ")))
        for i, t in enumerate(targets):
            if t >= src and t < src + size:
                if new_targets[i] != targets[i]:
                    raise Exception("sanity check: already set")
                new_targets[i] = dest + (t - src)
        

    print(new_targets)
    print(min(new_targets))


def problem2():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    seed_list = list(map(int, lines[0].split(" ")[1:]))
    # regroup targets into list of pairs
    targets = []
    for i in range(0, len(seed_list), 2):
        targets.append((seed_list[i], seed_list[i+1]))
    print(targets)
    new_targets = copy.deepcopy(targets)
    maps = lines[2:]

    for l in maps:
        if l == "":
            targets = new_targets
            new_targets = copy.deepcopy(targets)
            continue
        if ":" in l:
            print("processing", l)
            continue

        dest, src, size = list(map(int, l.split(" ")))
        i = 0
        while i < len(targets):
            print("processing", i, targets[i])
            rng = targets[i]
            t, t_size = rng

            # check if any overlap
            if t < src + size and t + t_size > src:
                # calculate size of leftovers
                before_size = max(0, src - t)
                after_size = max(0, t + t_size - (src + size))

                if new_targets[i][0] != targets[i][0]:
                    raise Exception("sanity check: already mapped this range")
                if before_size + after_size >= t_size:
                    raise Exception("sanity check: left over is bigger than entire range")

                # offset matching part, and reduce existing target to not match again
                offset = dest - src
                targets[i] = (t + before_size, t_size - before_size - after_size)
                new_targets[i] = (t + offset + before_size, t_size - before_size - after_size)

                # add leftovers for future mapping
                if before_size > 0:
                    targets.append((t, before_size))
                    new_targets.append((t, before_size))
                if after_size > 0:
                    targets.append((t + t_size - after_size, after_size))
                    new_targets.append((t + t_size - after_size, after_size))

            i += 1

    print(min(map(lambda x: x[0], new_targets)))


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
