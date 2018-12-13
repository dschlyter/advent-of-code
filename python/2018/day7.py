import sys

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
    # print(tasks - not_ready)

    dep = list(filter(lambda x: x[0] != best_task, dep))
    tasks = tasks - {best_task}

    # print(best_task, dep)
    if not tasks:
        return [best_task]
    else:
        return [best_task] + find_order(tasks, dep)


def problem2():
    lines = read_input()

    worker_count = 5

    dep = []
    for line in lines:
        s = line.split(" ")
        dep.append([s[1], s[7]])

    all_tasks = set([task for d in dep for task in d])
    t = solve2(all_tasks, dep, [None] * worker_count)
    print("best time", t)


def solve2(tasks, dep, work):
    steps = -1
    while tasks:
        steps += 1

        for i in range(len(work)):
            if work[i]:
                (time, task) = work[i]
                if time > 1:
                    work[i] = (time-1, task)
                else:
                    work[i] = None
                    dep = list(filter(lambda x: x[0] != task, dep))
                    tasks = tasks - {task}

            if not work[i]:
                ongoing = filter(lambda x: x, work)
                in_progress = set(list(map(lambda x: x[1], ongoing)))
                not_ready = set(map(lambda x: x[1], dep))
                best_tasks = list(sorted(tasks - not_ready - in_progress))

                if best_tasks:
                    pick = best_tasks[0]
                    work[i] = (task_time(pick), pick)
                    print(steps, i, pick)

    return steps


def run_workers(work):
    first = work[0]
    time_passed = max(first[0], 0)
    done_task = first[1]
    new_work = list(map(lambda t: (t[0] - time_passed, t[1]), work[1:]))

    return done_task, time_passed, new_work


def task_time(task):
    # return 1 + ord(task) - ord('A')
    return 61 + ord(task) - ord('A')


def read_input():
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()
