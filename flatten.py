import itertools

A = [
	list(range(5)),
	list(range(3)),
	list(range(4)),
]

print(list(itertools.chain.from_iterable(A)))
