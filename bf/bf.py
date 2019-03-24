import argparse
import sys
from getch import getch

def execute(program, stdout, stack_size=30000):
    p_ptr = 0  # program pointer
    s_ptr = 0  # stack pointer
    stack = [0 for x in range(stack_size)]
    jmp_stack = init_jump_stack(program)

    while p_ptr < len(program):
        c = program[p_ptr]
        if   c == '>': s_ptr += 1
        elif c == '<': s_ptr -= 1
        elif c == '+': stack[s_ptr] = (stack[s_ptr] + 1) % 256
        elif c == '-': stack[s_ptr] = (stack[s_ptr] - 1) % 256
        elif c == '.': print(chr(stack[s_ptr]), end='', flush=True, file=stdout)
        elif c == ',': stack[s_ptr] = ord(getch())
        elif c == '[' and stack[s_ptr] == 0: p_ptr = jmp_stack[p_ptr]
        elif c == ']' and stack[s_ptr] != 0: p_ptr = jmp_stack[p_ptr]
        p_ptr += 1

    print('', flush=True, file=stdout)

def init_jump_stack(program):
    jmp_stack = {}
    unmatched = []
    for i, c in enumerate(program):
        if c == '[':
            unmatched.append(i)
        elif c == ']':
            a, b = unmatched.pop(), i
            jmp_stack[a] = b
            jmp_stack[b] = a
    return jmp_stack

def main():
    parser = argparse.ArgumentParser(
        description='An interpreter for the Brainfuck programming language')
    parser.add_argument(
        'infile', nargs='?', type=argparse.FileType('r'), default=sys.stdin)
    args = parser.parse_args()

    program = args.infile.read()
    execute(program, sys.stdout)

main()
