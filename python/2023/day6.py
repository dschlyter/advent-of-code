import util
import copy
from collections import defaultdict

infile = 'input/day6.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    time = list(map(int, filter(lambda s: s, lines[0].split(" ")[1:])))
    dist = list(map(int, filter(lambda s: s, lines[1].split(" ")[1:])))

    ans = 1
    for i in range(len(time)):
        t, record = time[i], dist[i]
        ways = 0
        for speed in range(1, t):
            time_left = t - speed 
            covered = speed * time_left
            if covered > record:
                ways += 1
        print(t, record, ways)
        ans *= ways

    print(ans)


def problem2():
    pass

    with open(infile) as fp:
        lines = [l.strip() for l in fp]

    time = int(lines[0].split(":")[1].replace(" ", ""))
    dist = int(lines[1].split(":")[1].replace(" ", ""))

    print(time, dist)

    t, record = time, dist
    ways = 0
    for speed in range(1, t):
        time_left = t - speed 
        covered = speed * time_left
        if covered > record:
            ways += 1
    print(t, record, ways)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
