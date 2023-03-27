#!/usr/bin/env bash

FILENAME="$1"

rg 'https|^\d+\.' "$FILENAME" | cut -c-80 | tac | python3 -c '
import sys
import re
skip_count = 0
for line in sys.stdin.readlines():
    if skip_count != 0:
        skip_count -= 1
        continue
    if "%eval" in line:
        skip_count += 1
        continue
    if "http" in line:
        m = re.match("^.*(https:.*)\".*$", line)
        print(m.group(1))
' | tac > urls.txt
