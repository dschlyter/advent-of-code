import util

infile = 'input/day2.txt'


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
            lines = [l.strip() for l in fp]

    limits = {"red": 12, "green": 13, "blue": 14}

    answer = 0
    answer2 = 0

    for l in lines:
        game, hands = l.split(":")
        id = int(game.strip().split(" ")[1].strip())

        possible = True
        maxed = {"red": 0, "green": 0, "blue": 0}
        for h in hands.strip().split(";"):
            for cubes in h.strip().split(","):
                amount, color = cubes.strip().split(" ")
                if int(amount) > limits[color]:
                    print("Game", id, "is not possible", "because", cubes, "is", amount)
                    possible = False
                maxed[color] = max(maxed[color], int(amount))

        if possible:
            answer += id
        power = maxed["red"] * maxed["green"] * maxed["blue"]
        answer2 += power

    print("Part 1", answer)
    print("Part 2", answer2)


def problem2():
    pass

if __name__ == '__main__':
    main()
