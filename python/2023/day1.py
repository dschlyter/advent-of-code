import util

filename = 'input/day1.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(filename) as fp:
        lines = [l.strip() for l in fp]
            
    digits = list(map(str, range(10)))
    ans = 0

    for l in lines:
        first, last, = None, None
        for c in l:
            if c in digits:
                if first is None:
                    first = c
                last = c
        ans += int(first + last)

    print(ans)


def problem2():
    pass
    with open(filename) as fp:
        lines = [l.strip() for l in fp]
                
    digits = list(map(str, range(10)))
    digits2 = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    ans = 0

    for l in lines:
        first, last, = None, None
        for i, c in enumerate(l):
            if c in digits:
                if first is None:
                    first = c
                last = c
                continue
            for j, d2 in enumerate(digits2):
                if l[i:].startswith(d2):
                    if first is None:
                        first = str(j)
                    last = str(j)

        ans += int(first + last)

    print(ans)


if __name__ == '__main__':
    main()
