import pydash


def main():
    problem1()
    problem2()


def problem1():
    lines = read_input()
    print(lines)
    ans = pydash.chain(lines) \
        .map(lambda x: x.replace("+", "")) \
        .map(lambda x: int(x)) \
        .reduce(lambda a,b: a+b) \
        .value()

    print(ans)


def problem2():
    pass


def read_input():
    with open('input/day1.txt') as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()