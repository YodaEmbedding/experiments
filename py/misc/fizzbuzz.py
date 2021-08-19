#!/usr/bin/env python3

# for x in range(1, 101):
#     print(x, end=' ')
#     if x % 15 == 0:
#         print('FizzBuzz')
#     elif x % 3 == 0:
#         print('Fizz')
#     elif x % 5 == 0:
#         print('Buzz')
#     else:
#         print('')


def fizz_buzz(x):
    return (
        " FizzBuzz"
        if x % 15 == 0
        else " Fizz"
        if x % 3 == 0
        else " Buzz"
        if x % 5 == 0
        else ""
    )


print("\n".join(str(x) + fizz_buzz(x) for x in range(1, 101)))
