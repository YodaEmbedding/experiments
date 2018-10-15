# https://stackoverflow.com/questions/52572613/remove-john-wick-from-a-list-using-double-recursion/52573100#52573100

def purge(env, target, pos=None):
    if pos is None:
        pos = [0]

    for x in env:
        if pos[0] >= len(target):
            yield x
        if isinstance(x, list):
            x = list(purge(x, target, pos=pos))
            yield x
        elif x != target[pos[0]]:
            yield x
        else:
            pos[0] += 1

env = ["a", ["j", "e", "o"], ["h", "n", "s", "w", "o"], ["i", "c", "k"]]

print(list(purge(env, "johnwick")))
