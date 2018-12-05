import pydash

filename = 'input/day4.txt'


def main():
    problem1()
    problem2()


def problem1():
    lines = read_input()
    sleeps = parse(lines)

    s = sorted(sleeps.items(), key=lambda x: time_asleep(x[1]))
    most_sleepy = s[-1]
    m = sleep_map(most_sleepy[1])
    print(m, len(m))
    print(max_min(m) * most_sleepy[0])


def time_asleep(sleeps):
    sum = 0
    for sleep in sleeps:
        sum += sleep[1] - sleep[0]
    return sum


def sleep_map(sleeps):
    m = [0] * 60
    for sleep in sleeps:
        for i in range(sleep[0], sleep[1]):
            m[i] += 1
    return m


def max_min(sleep_map):
    max = 0
    index = 0
    for i in range(len(sleep_map)):
        if sleep_map[i] > max:
            max = sleep_map[i]
            index = i
    return index


def problem2():
    lines = read_input()
    sleeps = parse(lines)
    sleep_maps = pydash.map_values(sleeps, sleep_map)
    sorted_minutes = list(sorted(flatten_minutes(sleep_maps), key=lambda s: s[2]))
    best = sorted_minutes[-1]
    print(best[0] * best[1])


def flatten_minutes(sleep_maps):
    for guard, sleep_map in sleep_maps.items():
        for i in range(len(sleep_map)):
            yield (guard, i, sleep_map[i])


def parse(lines):
    sleeps = {}
    guard = 0
    start = 0
    for line in lines:
        if "Guard" in line:
            guard = int(line.split(" ")[3].replace("#", ""))
        else:
            time = int(line.split(" ")[1].split(":")[1].replace("]", ""))
            if "falls asleep" in line:
                start = time
            elif "wakes up" in line:
                sleeps[guard] = sleeps.get(guard, []) + [[start, time]]
            else:
                raise Exception("oh noes")

    return sleeps


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
