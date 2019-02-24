#!/usr/bin/env python3
# https://www.codewars.com/kata/text-align-justify

from math import floor
from itertools import chain

def justify(text, width):
    def join_line(line):
        if len(line) < 2:
            return ''.join(line)

        num_gaps = len(line) - 1
        num_chars = sum(map(len, line))
        num_spaces = width - num_chars

        gap_size = int(floor(num_spaces / num_gaps))
        num_big = num_spaces - gap_size * num_gaps + 1

        big_gap = ' ' * (gap_size + 1)
        small_gap = ' ' * gap_size

        return small_gap.join(chain(
            [big_gap.join(line[:num_big])],
            line[num_big:]))

    words = text.split()
    lines = [[]]
    curr_width = 0

    for word in words:
        word_len = len(word) + 1 if curr_width > 0 else len(word)

        if curr_width + word_len > width:
            lines.append([word])
            curr_width = len(word)
            continue

        lines[-1].append(word)
        curr_width += word_len

    return '\n'.join(chain(
        map(join_line, lines[:-1]),
        [' '.join(lines[-1])]))
