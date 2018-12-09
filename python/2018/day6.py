import pydash

from string import ascii_lowercase

filename = 'input/day5.txt'


def main():
    problem1()
    problem2()


def problem1():
    lines = read_input()
    print("pre", len(lines[0]))
    out = react(lines[0])
    print(len(out))


def react(line):
    reacted = True
    curr = line
    while reacted:
        reacted = False
        after = []
        i = 0
        while i < len(curr):
            if not after:
                after.append(curr[i])
                i += 1
            a, b = after[-1], curr[i]
            if a != b and a.lower() == b.lower():
                after = after[:-1]
            else:
                after.append(curr[i])
            i += 1
        curr = after
    return curr


def problem2():
    lines = read_input()
    results = []
    for c in ascii_lowercase:
        mod = lines[0]
        mod = mod.replace(c, "")
        mod = mod.replace(c.upper(), "")
        out = react(mod)
        results.append((c, len(out)))
    s = sorted(results, key=lambda s: s[1])
    print(s)


def read_input():
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()
