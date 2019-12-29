from _collections import deque


# Stolen intcode computer


class ProgramTerminatedError(Exception):
    pass


class NoInputProvidedError(Exception):
    pass


class IntCode:

    def __init__(self, program):
        self.program = program.copy()
        # noinspection PyArgumentList
        self.position = 0
        self.base_offset = 0
        self.input_instructions = deque()
        self.output_values = deque()
        self.halted = False

    def run(self, suspend_on_output=False, suspend_on_input=False):
        # self.program = self.program + ([0] * 1000)

        while True:
            instruction = self.program[self.position]
            if instruction == 99:
                self.halted = True
                break
            opcode = instruction % 10

            # func_call = self.operations[opcode]
            # func_call(instruction)

            if opcode == 1:
                mode_p_ = int(instruction / 100) % 10
                mode_p2 = int(instruction / 1000) % 10
                mode_p3 = int(instruction / 10000) % 10
                p_8 = self.get_value(mode_p_, 1)
                p_7 = self.get_value(mode_p2, 2)
                pos2 = self.get_store_pos(mode_p3, 3)
                result = p_8, p_7, pos2
                value_p1, value_p2, store_pos = result
                self.store(store_pos, value_p1 + value_p2)
                self.position += 4
            if opcode == 2:
                p_10 = int(instruction / 100) % 10
                p_13 = int(instruction / 1000) % 10
                p_12 = int(instruction / 10000) % 10
                p_11 = self.get_value(p_10, 1)
                p_9 = self.get_value(p_13, 2)
                pos3 = self.get_store_pos(p_12, 3)
                result1 = p_11, p_9, pos3
                value_p_, p_, pos = result1
                self.store(pos, value_p_ * p_)
                self.position += 4
            if opcode == 3:
                mode_p1 = parse_instruction(instruction, 1)
                pos1 = self.get_store_pos(mode_p1, 1)
                try:
                    self.store(pos1, self.input_instructions.popleft())
                    self.position += 2
                except IndexError:
                    if suspend_on_input:
                        return
                    raise NoInputProvidedError
            if opcode == 4:
                p_1 = self.get_one_value(instruction)
                self.output_values.append(p_1)
                self.position += 2
            if opcode == 5:
                p_3, p_2 = self.get_two_values(instruction)
                self.position = p_2 if p_3 else self.position + 3
            if opcode == 6:
                p_6, p_5 = self.get_two_values(instruction)
                self.position = p_5 if not p_6 else self.position + 3
            if opcode == 7:
                p_15, p_14, pos4 = self.get_two_values_and_store_pos(instruction)
                self.store(pos4, 1 if p_15 < p_14 else 0)
                self.position += 4
            if opcode == 8:
                p_17, p_16, pos5 = self.get_two_values_and_store_pos(instruction)
                self.store(pos5, 1 if p_17 == p_16 else 0)
                self.position += 4
            if opcode == 9:
                p_4 = self.get_one_value(instruction)
                self.base_offset += p_4
                self.position += 2

            if opcode == 4 and suspend_on_output:
                return
        return

    def store_one_or_zero(self, instruction, function):
        value_p1, value_p2, store_pos = self.get_two_values_and_store_pos(instruction)
        self.store(store_pos, 1 if function(value_p1, value_p2) else 0)
        self.position += 4

    def adjust_base(self, instruction):
        value_p1 = self.get_one_value(instruction)
        self.base_offset += value_p1
        self.position += 2

    def get_value(self, mode, position_offset):
        value_at_pos = self.program[self.position + position_offset]
        if mode == 0:
            self.grow_memory(value_at_pos)
            return self.program[value_at_pos]
        elif mode == 1:
            return value_at_pos
        elif mode == 2:
            value_base_offset_adj = self.base_offset + value_at_pos
            self.grow_memory(value_base_offset_adj)
            return self.program[value_base_offset_adj]

    def store(self, store_pos, value):
        self.grow_memory(store_pos)
        self.program[store_pos] = value

    def grow_memory(self, memory_size):
        if len(self.program) <= memory_size:
            self.program = self.program + ([0] * memory_size)

    def get_store_pos(self, mode, position_offset):
        if mode == 0:
            return self.program[self.position + position_offset]
        elif mode == 2:
            return self.base_offset + self.program[self.position + position_offset]

    def get_one_value(self, instruction):
        mode_p1 = parse_instruction(instruction, 1)
        value_p1 = self.get_value(mode_p1, 1)
        return value_p1

    def get_two_values(self, instruction):
        mode_p1, mode_p2 = parse_instruction(instruction, 2)
        value_p1 = self.get_value(mode_p1, 1)
        value_p2 = self.get_value(mode_p2, 2)
        return value_p1, value_p2

    def get_two_values_and_store_pos(self, instruction):
        mode_p1, mode_p2, mode_p3 = parse_instruction(instruction)
        value_p1 = self.get_value(mode_p1, 1)
        value_p2 = self.get_value(mode_p2, 2)
        store_pos = self.get_store_pos(mode_p3, 3)
        return value_p1, value_p2, store_pos

    def queue_input(self, value):
        self.input_instructions.append(value)

    def get_last_output(self):
        self.output_values.pop()

    def run_until_output(self):
        if self.output_values:
            return self.output_values.pop()
        self.run(True)
        if self.output_values:
            return self.output_values.pop()
        else:
            raise ProgramTerminatedError


def parse_instruction(instruction, modes_required=3):
    param_1_mode = int(instruction / 100) % 10
    if modes_required == 1:
        return param_1_mode
    param_2_mode = int(instruction / 1000) % 10
    if modes_required == 2:
        return param_1_mode, param_2_mode
    param_3_mode = int(instruction / 10000) % 10
    return param_1_mode, param_2_mode, param_3_mode
