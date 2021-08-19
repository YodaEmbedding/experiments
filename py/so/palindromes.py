import os


def is_palindrome(x):
    return x == x[::-1]


with open("so_palindromes.txt", "r") as f:
    for line in map(lambda x: x.rstrip(), f):
        if is_palindrome(line):
            print(line)
