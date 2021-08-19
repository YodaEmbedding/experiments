# MK3WA3-KF3

import re

def validate(line):
	return re.match(r"^\s*(\+|-)?((1\d{1,9})|([1-9]\d{0,8})|0)\s*$", line)

def solution(file_object):
	for line in file_object:
		if validate(line):
			yield int(line.strip())

file_object = open('codility_1.txt', 'r')

for i in solution(file_object):
	print(i)

# Compilation successful.
# 
# Your test case: ['137\n-104\n2 58\n  +0\n++3\n+1\n 23.9\n2000000000\n-0\nfive\n -1\n']
# Returned value: [137, -104, 0, 1, 0, -1]
# 
# Example test:   '137\n-104\n2 58\n  +0\n++3\n+1\n 23.9\n2000000000\n-0\nfive\n -1\n'
# OK
