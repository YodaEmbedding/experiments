grid = [[0, 0, 0, 4], [0, 0, 4, 2], [2, 4, 4, 2], [0, 8, 4, 2]]

pairs = [
    (i, j) for i, row in enumerate(grid) for j, n in enumerate(row) if n == 0
]

print(pairs)
