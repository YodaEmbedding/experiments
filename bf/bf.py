import argparse
import sys
from getch import getch

def execute(program, stdout):
    p_ptr = 0  # program pointer
    s_ptr = 0  # stack pointer
    stack = [0]
    jmp_stack = init_jump_stack(program)
    printc = lambda *args: print(*args, end='', flush=True, file=stdout)
    endl = lambda: program.find('\n', p_ptr)
    decode = lambda s: s.encode().decode('unicode_escape')

    def dump_state():
        stack_str = '\n'.join(f'{i}: {x}' for i, x in enumerate(stack))
        printc(
            '\n'
            f'Program pointer: #{p_ptr}, {program[p_ptr]}\n'
            f'Stack pointer:   #{s_ptr}, {stack[s_ptr]}, {chr(stack[s_ptr])}\n'
            f'Stack:\n{stack_str}\n')

    while p_ptr < len(program):
        c = program[p_ptr]
        if   c == '>': s_ptr += 1; (s_ptr == len(stack) and stack.append(0))
        elif c == '<': s_ptr -= 1
        elif c == '+': stack[s_ptr] = (stack[s_ptr] + 1) % 256
        elif c == '-': stack[s_ptr] = (stack[s_ptr] - 1) % 256
        elif c == '.': printc(chr(stack[s_ptr]))
        elif c == ',': stack[s_ptr] = ord(getch())
        elif c == '[' and stack[s_ptr] == 0: p_ptr = jmp_stack[p_ptr]
        elif c == ']' and stack[s_ptr] != 0: p_ptr = jmp_stack[p_ptr]
        elif c == '@': printc(decode(program[p_ptr+1:endl()])); p_ptr = endl()
        elif c == '#' or c == ';': p_ptr = endl()
        elif c == '%': printc(stack[s_ptr])
        elif c == '?': dump_state()
        p_ptr += 1

    printc('\n')

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
        description=(
            'An interpreter for the Brainfuck programming language.\n'
            '\n'
            'Instructions:\n'
            '  >  increment data pointer\n'
            '  <  decrement data pointer\n'
            '  +  increment data pointer contents\n'
            '  -  decrement data pointer contents\n'
            '  .  echo data pointer contents as character\n'
            '  ,  input character into data pointer contents\n'
            '  [  jump to matching ] if data pointer contents are zero\n'
            '  ]  jump to matching [ if data pointer contents are nonzero\n'
            '\n'
            'Nonstandard extensions:\n'
            '  ;  comment\n'
            '  #  comment\n'
            '  @  echo following text\n'
            '  %  echo data pointer contents as number\n'
            '  ?  echo program state dump\n'
        ),
        formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument(
        'infile', nargs='?', type=argparse.FileType('r'), default=sys.stdin)
    args = parser.parse_args()

    program = args.infile.read()
    execute(program, sys.stdout)

main()
