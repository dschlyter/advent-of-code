import util
import copy
import math
import sys
from collections import defaultdict
from queue import Queue

suffix = sys.argv[1] if len(sys.argv) > 1 else ''
infile = f'input/day13{suffix}.txt'

sys.setrecursionlimit(20_000)


def main():
    problem1()
    problem2()


def problem1():
    with open(infile) as fp:
        lines = [l.strip() for l in fp]
    lines.append("")

    ans = 0
    img = []
    for l in lines:
        if l:
            img.append(l)
        else:
            for r in img:
                print(r)

            h = href(img)
            v = vref(img)
            if h is not None:
                ans += h * 100
            if v is not None:
                ans += v
            print(h, v, ans)
            print()

            img = []

    print(ans)


def href(img, ignore=None):
    for i in range(len(img) - 1):
        reflection = True
        for j in range(len(img)):
            i1 = i - j
            i2 = i + j + 1
            if i1 < 0 or i2 >= len(img):
                break
            if img[i1] != img[i2]:
                reflection = False
                break
        if reflection and i + 1 != ignore:
            return i + 1
    return None


def vref(img, ignore=None):
    transposed = []
    for i in range(len(img[0])):
        l = ""
        for j in range(len(img)):
            l += img[j][i]
        transposed.append(l)
    return href(transposed, ignore)


def problem2():
    with open(infile) as fp:
        lines = [[c for c in l.strip()] for l in fp]
    lines.append("")

    swap = {
        "#": ".",
        ".": "#",
    }

    ans = 0
    img = []
    for l in lines:
        if l:
            img.append(l)
            continue

        for r in img:
            print(r)

        original_h = href(img)
        original_v = vref(img)
        print("original", original_h, original_v)
        print()

        answer = None
        for y in range(len(img)):
            for x in range(len(img[0])):
                img[y][x] = swap[img[y][x]]

                new_ans = 0
                h = href(img, original_h)
                v = vref(img, original_v)
                if h is not None:
                    new_ans += h * 100
                if v is not None:
                    new_ans += v
                if new_ans > 0:
                    if answer is None:
                        answer = new_ans
                        ans += new_ans

                        for r in img:
                            print(r)
                        print(y, x, h, v, ans)
                    elif answer != new_ans:
                        raise Exception("sanity check: multiple answers")

                img[y][x] = swap[img[y][x]]
        if answer is None:
            raise Exception("sanity check: no answer")
        print()

        img = []

    print(ans)


def get(l, i):
    if l is None:
        return None
    if i < 0 or i >= len(l):
        return None
    return l[i]

if __name__ == '__main__':
    main()
