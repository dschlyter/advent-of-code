import re
import time


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
    tokens = re.sub(filter_regex, "", line).split(split_token)
    no_empty = list(filter(lambda x: len(x) > 0, tokens))
    return no_empty


# credit https://stackoverflow.com/questions/5478351/python-time-measure-function
def timing(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        print('{:s} function took {:.3f} ms'.format(f.__name__, (time2-time1)*1000.0))

        return ret
    return wrap
