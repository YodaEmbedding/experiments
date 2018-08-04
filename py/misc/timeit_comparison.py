import timeit

# Testing https://stackoverflow.com/a/16726462/365102

s = (-1, 1)

sign1 = lambda x: 1 if x >= 0 else -1
sign2 = lambda x: (-1, 1)[x >= 0]
sign3 = lambda x: s[x >= 0]

sign4 = lambda x: x and (1 if x >= 0 else -1)
sign5 = lambda x: x and (-1, 1)[x >= 0]
sign6 = lambda x: x and s[x >= 0]

sign1_time = timeit.repeat("for x in range(-10, 10): sign1(x)", "from __main__ import sign1", number=100000)
sign2_time = timeit.repeat("for x in range(-10, 10): sign2(x)", "from __main__ import sign2", number=100000)
sign3_time = timeit.repeat("for x in range(-10, 10): sign3(x)", "from __main__ import sign3", number=100000)
sign4_time = timeit.repeat("for x in range(-10, 10): sign4(x)", "from __main__ import sign4", number=100000)
sign5_time = timeit.repeat("for x in range(-10, 10): sign5(x)", "from __main__ import sign5", number=100000)
sign6_time = timeit.repeat("for x in range(-10, 10): sign6(x)", "from __main__ import sign6", number=100000)

print('sign1: {}'.format(sign1_time))
print('sign2: {}'.format(sign2_time))
print('sign3: {}'.format(sign3_time))
print('sign4: {}'.format(sign4_time))
print('sign5: {}'.format(sign5_time))
print('sign6: {}'.format(sign6_time))
