import itertools
from functools import reduce
from typing import List, Callable

def generateSequence(seed, f, n):
    sequence = list(seed)
    for i in range(n):
        sequence.append(f(sequence, i))
    return sequence

def lengthOfChitterSequence():
	a = 0
	yield a

	while True:
		a = (a + 1) * 2
		yield a

def chitterSequence(n: int):
	chitterList = []
	for i in range(n):
		chitterList.append(i)
		chitterList = chitterList * 2
	return chitterList

print(generateSequence([0], lambda x, i: (x[-1] + 1) * 2, 9))

generator = lengthOfChitterSequence()
print(list(itertools.islice(generator, 0, 16)))

for i in range(6):
	print(chitterSequence(i))


