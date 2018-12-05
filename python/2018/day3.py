import pydash

filename = 'input/day3.txt'


def main():
    problem1()
    problem2()


def problem1():
    lines = read_input()
    claims = parse_claims(lines)
    _, count = count_claims(claims)

    print(count)


def problem2():
    lines = read_input()
    claims = parse_claims(lines)
    fabric, _ = count_claims(claims)

    id = 0
    for claim in claims:
        id += 1
        pos = claim[0]
        size = claim[1]
        conflict = False
        for i in range(int(size[0])):
            for j in range(int(size[1])):
                x = int(pos[0]) + i
                y = int(pos[1]) + j
                if fabric[x][y] > 1:
                    conflict = True
        if not conflict:
            print(id)


def parse_claims(lines):
    claims = []
    for line in lines:
        s = line.split(" ")
        claim = (s[2].replace(":", "").split(","), s[3].split("x"))
        claims.append(claim)
    return claims


def count_claims(claims):
    fabric = []
    for i in range(1000):
        fabric.append([0] * 1000)

    for claim in claims:
        pos = claim[0]
        size = claim[1]
        for i in range(int(size[0])):
            for j in range(int(size[1])):
                x = int(pos[0]) + i
                y = int(pos[1]) + j
                fabric[x][y] += 1

    count = 0
    for i in range(1000):
        for j in range(1000):
            if fabric[i][j] > 1:
                count += 1
    return fabric, count


def read_input():
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()
