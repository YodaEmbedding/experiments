#!/usr/bin/env python3

import os
import sys

with open("txt_smile.txt") as f:
    lines = f.read().splitlines()

text_coords = [(s.split(" ")) for s in lines[2:]]
coords = [(int(x), int(y)) for x, y in text_coords]

print(lines[2:])
print(text_coords)
print(coords)
