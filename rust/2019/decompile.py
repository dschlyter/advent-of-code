#!/usr/bin/env python3

# Rudimentary decompiler for intcode for advent of code 2019

import sys
from collections import namedtuple

Op = namedtuple("Op", "name arg_count")


class Inst:
    def __init__(self, location, name, opinst, args, indent):
        self.location = location
        self.name = name
        self.alt_name = None
        self.opinst =opinst
        self.args = args
        self.indent = indent

    def repr_name(self):
        return self.alt_name or self.name

    def repr_args(self):
        if self.name == 'memory':
            return " ".join(map(str, self.args))
        ret = []
        for i, arg in enumerate(self.args):
            mod_pos = i+2
            mode = int((self.opinst / (10 ** mod_pos)) % 10)
            mode_char = "&" if mode == 0 else ("?" if mode == 2 else "")
            ret.append(mode_char+str(arg))
        return " ".join(ret)

    def is_jump(self):
        return self.name in ["jump", "zjump"]

    def jump_range(self):
        if not self.is_jump():
            raise Exception("only valid for jumps")
        jump_target = self.args[1]
        return min(self.location, jump_target), max(self.location, jump_target)


def main():
    file = sys.argv[1]

    with open(file) as f:
        for line in f:
            program = line
            break

    codes = list(map(int, program.split(",")))
    ops = {
        1: Op("add", 3),
        2: Op("mul", 3),
        3: Op("input", 1),
        4: Op("output", 1),
        5: Op("jump", 2),
        6: Op("zjump", 2),
        7: Op("lt", 3),
        8: Op("eq", 3),
        9: Op("add_rel_base", 1),
        99: Op("halt", 0)
    }

    instructions = []

    i = 0
    while i < len(codes):
        op_inst = codes[i]
        op_code = op_inst % 100

        # assume that invalid instructions are memory
        if op_code not in ops:
            instructions.append(Inst(i, "memory", None, [op_inst], 0))
            i += 1
            continue

        op = ops[op_code]
        args = codes[(i+1):(i+1+op.arg_count)]
        instructions.append(Inst(i, op.name, op_inst, args, 0))
        i += 1 + op.arg_count

    # Best effort indent
    # Area covered by a jump is a block. Two blocks may conflict if they jump into each other.
    # Remove the worst blocks until there are no conflicts left.
    jump_instructions = [inst for inst in instructions if inst.is_jump()]

    def conflict(i1, i2):
        s1, e1 = i1.jump_range()
        s2, e2 = i2.jump_range()
        return (s1 < s2 < e1 < e2) or (s2 < s1 < e2 < e1)

    def remove_conflicts(jump_instructions):
        print("Removing block conflicts,", len(jump_instructions), "jump instructions left")
        conflict_count = {}
        for i1 in jump_instructions:
            conflict_count[i1.location] = 0
            for i2 in jump_instructions:
                if conflict(i1, i2):
                    conflict_count[i1.location] += 1

        conflict_toplist = sorted(jump_instructions, key=lambda x: conflict_count[x.location], reverse=True)
        if not conflict_toplist:
            return []
        if conflict_count[conflict_toplist[0].location] == 0:
            return conflict_toplist
        else:
            return remove_conflicts(conflict_toplist[1:])

    valid_jump_blocks = remove_conflicts(jump_instructions)

    for inst in valid_jump_blocks:
        start, stop = inst.jump_range()
        for other_inst in instructions:
            if start < other_inst.location < stop:
                other_inst.indent += 1

        # forward jump block can be replaced with if
        if inst.location == start:
            inst.alt_name = {"jump": "if", "zjump": "ifz"}[inst.name]
            inst.args = inst.args[0:1]

        # backward jump block can be replaced with while (as in do-while)
        if inst.location == stop:
            # a clean forward jump can be replaced with an if statement
            inst.alt_name = {"jump": "while", "zjump": "whilez"}[inst.name]
            inst.args = inst.args[0:1]

    # List used labels (note: will miss dynamic targets)
    # We also skip ifs and whiles that have valid blocks
    used_targets = {inst.args[1]
                    for inst in instructions
                    if not inst.alt_name and inst.is_jump()}

    for inst in instructions:
        # print(f"{inst.indent*'  '}{inst.location}: {inst.name} {inst.opinst} {inst.repr_args()} {inst.args}")
        target_label = str(inst.location)+': ' if inst.location in used_targets else ''
        print(f"{inst.indent*'  '}{target_label}{inst.repr_name()} {inst.repr_args()}")

    print(ops[99].name)


if __name__ == '__main__':
    main()
