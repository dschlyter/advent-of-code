import re
import sys
import time
from pathlib import Path


def main(filename, part1, part2):
    parts, inputs = parse_args(filename)

    for input_file in inputs:
        print("Running", input_file)
        if 1 in parts:
            lines = read_input(input_file)
            part1(lines)
        if 2 in parts:
            lines = read_input(input_file)
            part2(lines)


def parse_args(filename):
    args = sys.argv[1:]

    parts = {1, 2}
    if get(args, 0) == "1":
        parts = {1}
        args = args[1:]
    elif get(args, 0) == "2":
        parts = {2}
        args = args[1:]

    if args:
        inputs = args
    else:
        day = filename.split("/")[-1].replace(".py", "")
        inputs = sorted([file for file in map(str, Path('input').glob(f'*.txt'))
                         if f'{day}.txt' in file or f'{day}_' in file])

    return parts, inputs


def get(coll, key, default_value=None):
    if isinstance(coll, (list, str)):
        if len(coll) > key:
            return coll[key]
        return default_value
    raise Exception("unsupported col")


def get_in(coll, *keys, default_value=None):
    key = keys[0]
    keys = keys[1:]
    if isinstance(coll, (list, str)):
        if len(coll) > key:
            res = coll[key]
            if not keys:
                return res
            else:
                return get_in(res, *keys, default_value=default_value)
        return default_value
    raise Exception("unsupported col")


def read_input(filename):
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


def parse_ints(filename):
    lines = read_input(filename)
    ret = []
    for line in lines:
        ret.append(parse_int_line(line))
    return ret


def parse_int_line(line):
    t = parse_token_line(line, "[^-0-9 ]", " ")
    return list(map(int, t))


def parse_tokens(filename, filter_regex, split_token):
    lines = read_input(filename)
    ret = []
    for line in lines:
        ret.append(parse_token_line(line, filter_regex, split_token))
    return ret


def parse_token_line(line, filter_regex, split_token):
    tokens = re.sub(filter_regex, split_token, line).split(split_token)
    no_empty = list(filter(lambda x: len(x) > 0, tokens))
    return no_empty


def print_states(*sets, flip=False):
    m = {}
    for i in range(len(sets) // 2):
        s = sets[2*i]
        token = sets[2*i+1]
        for coord in s:
            m[coord] = token
    print_state(m, flip)


def print_state(state_map, flip=False):
    if flip:
        new_map = {}
        for k, v in state_map.items():
            (x, y) = k
            new_map[(y, x)] = v
        state_map = new_map

    xmin, xmax, ymin, ymax = sys.maxsize, -sys.maxsize, sys.maxsize, -sys.maxsize
    for k in state_map.keys():
        (y, x) = k
        xmin = min(xmin, x)
        xmax = max(xmax, x)
        ymin = min(ymin, y)
        ymax = max(ymax, y)
    for y in range(ymin, ymax+1):
        for x in range(xmin, xmax+1):
            print(state_map.get((y, x), " "), end="")
        print()


# credit https://stackoverflow.com/questions/5478351/python-time-measure-function
def timing(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        print('{:s} function took {:.3f} ms'.format(f.__name__, (time2-time1)*1000.0))

        return ret
    return wrap
