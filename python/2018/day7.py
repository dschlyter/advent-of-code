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
    print(tasks - not_ready)

    dep = list(filter(lambda x: x[0] != best_task, dep))
    tasks = tasks - {best_task}

    # print(best_task, dep)
    if not tasks:
        return [best_task]
    else:
        return [best_task] + find_order(tasks, dep)


def problem2():
    lines = read_input()

    worker_count = 2

    dep = []
    for line in lines:
        s = line.split(" ")
        dep.append([s[1], s[7]])

    all_tasks = set([task for d in dep for task in d])
    t = dfs(all_tasks, dep, worker_count, [])
    print("best time", t)


def dfs(tasks, dep, work):
    if not tasks:
        return 0

    # time_passed = 0
    # if not idle_workers:
        # raise "There should be idles"
        # done_task, time_passed, work = run_workers(work)
        # tasks = tasks - done_task

    time_passed = workers_time_left[0]
    workers_time_left = list(map(lambda x: x-time_passed, workers_time_left[1:]))

    not_ready = set(map(lambda x: x[1], dep))
    ready = tasks - not_ready

    best_time = sys.maxsize
    for selected_task in ready:
        new_dep = list(filter(lambda x: x[0] != selected_task, dep))
        new_tasks = tasks - {selected_task}
        new_workers = list(sorted(workers_time_left + [task_time(selected_task)]))

        time = dfs(new_tasks, new_dep, new_workers)
        best_time = min(time, best_time)

    print(tasks, "complete in", time_passed + best_time)

    return time_passed + best_time

def run_workers(work):
    first = work[0]
    time_passed = max(first[0], 0)
    done_task = first[1]
    new_work = list(map(lambda t: (t[0] - time_passed, t[1]), work[1:]))

    return done_task, time_passed, new_work


def task_time(task):
    return 1 + ord(task) - ord('A')
    # return 61 + ord(task) - ord('A')


def read_input():
    with open(filename) as file:
        lines = file.read().split("\n")
        if lines[-1] == '':
            lines = lines[:-1]
        return lines


if __name__ == '__main__':
    main()
