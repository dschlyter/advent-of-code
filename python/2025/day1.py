import util
import sys

from util import get

def main():
    p1()
    p2()

def p1():
    pos = 50
    count_zero = 0
    lines = util.read_input(get(sys.argv, 1, "input/day1.txt"))
    for line in lines:
        direction = line[0]
        steps = int(line[1:])
        if direction == 'L':
            pos -= steps
        elif direction == 'R':
            pos += steps

        pos = pos % 100

        if pos == 0:
            count_zero += 1

    print("Part 1:", count_zero)


def p2():
    pos = 50
    count_zero = 0
    lines = util.read_input(get(sys.argv, 1, "input/day1.txt"))
    for line in lines:
        direction = line[0]
        steps = int(line[1:])
        if direction == 'L':
            new_pos = pos - steps
        elif direction == 'R':
            new_pos = pos + steps

        count_zero += abs(pos // 100 - new_pos // 100)
        # hack to not double count or undercount zero
        if direction == 'L' and new_pos % 100 == 0:
            count_zero += 1
        if direction == 'L' and pos % 100 == 0:
            count_zero -= 1

        pos = new_pos % 100

    print("Part 1:", count_zero)

if __name__ == "__main__":
    main()