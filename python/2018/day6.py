import pydash

from string import ascii_lowercase
from queue import Queue

filename = 'input/day6.txt'


def main():
    problem1()
    problem2()


def problem1():
    lines = read_input()

    points = []
    for line in lines:
        s = line.replace(",", "").split(" ")
        print(s)
        points.append(list(map(int, s)))

    grid = [[0] * 1000] * 1000
    print(grid)

    q = Queue()
    i = 1
    for p in points:
        grid[p[0]][p[1]] = i
        q.put((p, i))

    while not q.empty():
        (p, c) = q.get()



def problem2():
    lines = read_input()


def read_input():
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()
