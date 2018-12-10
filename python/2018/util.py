import re


def read_input(filename):
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


def parse_ints(filename):
    token_lines = parse_tokens(filename, "[^-0-9 ]", " ")
    ret = []
    for t in token_lines:
        print(t)
        ret.append(list(map(int, t)))
    return ret


def parse_tokens(filename, filter_regex, split_token):
    lines = read_input(filename)
    ret = []
    for line in lines:
        tokens = re.sub(filter_regex, "", line).split(split_token)
        no_empty = list(filter(lambda x: len(x) > 0, tokens))
        ret.append(no_empty)
    return ret
