import pydash

from string import ascii_lowercase
from queue import Queue

filename = 'input/day7.txt'


def main():
    problem1()
    problem2()


def problem1():
    lines = read_input()

    dep = []
    for line in lines:
        s = line.split(" ")
        dep.append([s[1], s[7]])
    all_tasks = set([task for d in dep for task in d])
    o = find_order(all_tasks, dep)
    print("".join(o))


def find_order(tasks, dep):
    not_ready = set(map(lambda x: x[1], dep))
    best_task = list(sorted(tasks - not_ready))[0]

    dep = list(filter(lambda x: x[0] != best_task, dep))
    tasks = tasks - {best_task}

    print(best_task, dep)
    if not tasks:
        return [best_task]
    else:
        return [best_task] + find_order(tasks, dep)


def problem2():
    lines = read_input()


def task_time(task):
    61 + ord(task) - ord('A')


def read_input():
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()
