def extend(myList, n):
    extensions = [range(x, x + n) for x in myList]
    return [item for sublist in extensions for item in sublist]  # flatten


print(extend([4, 8, 4, 10], 4))
