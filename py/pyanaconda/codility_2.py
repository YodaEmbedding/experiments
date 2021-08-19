from itertools import groupby

def nchoose2(n):
	return n * (n - 1) / 2

def solution(A):
	ans = sum([nchoose2(len(list(group))) for key, group in groupby(sorted(A))])
	return int(ans) if ans <= 1000000000 else 1000000000

test = [0, 1, 2, 5, 2, 4, 5, 6, 6, 6, 1, 3, 5, 5]

# 1 + 1 + (4 choose 2) + (3 choose 2) = 11
print(solution(test))


