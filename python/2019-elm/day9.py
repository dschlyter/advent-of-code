from intcode import IntCode


def boost_keycode(program, input_value):
    computer = IntCode(program)
    if input_value:
       computer.queue_input(input_value)
    computer.run()
    return list(computer.output_values)


if __name__ == '__main__':
    # with open('input') as f:
        # print(boost_keycode([int(instr) for instr in f.readline().split(',')], 1))

    with open('input') as f:
        print(boost_keycode([int(instr) for instr in f.readline().split(',')], 2))

# timing experiments:
# python3.8.0 - 0.425 s