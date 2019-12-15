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

# timing experiments - benchmarked with hyperfine

# python3.8.0 - 0.424 s
# pypy3 - 0.137

# after unrolling the loop
# python3.8.0 - 0.460 s (slower? hmm)
# pypy3 - 0.106

# after removing the lambda in addition and multiplication
# python3.8.0 - 0.447 s
# pypy3 - 0.097

# after removing extra memory and just expanding the array
# python3.8.0 - 0.446 s
# pypy3 - 0.096

# static memory allocation
# python3.8.0 - 0.430 s
# pypy3 - 0.097

# dynamic memory allocation
# python3.8.0 - 0.467
# pypy3 - 0.098

# inline all the things
# python3.8.0 - 0.404
# pypy3 - 0.96
