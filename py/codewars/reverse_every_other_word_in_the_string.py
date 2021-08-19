#!/usr/bin/env python3
# https://www.codewars.com/kata/reverse-every-other-word-in-the-string


def reverse_alternate(s):
    return " ".join(
        x if i % 2 == 0 else x[::-1] for i, x in enumerate(s.split())
    )
