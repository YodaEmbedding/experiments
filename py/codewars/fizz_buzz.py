#!/usr/bin/env python3
# https://www.codewars.com/kata/fizz-buzz

def fizzbuzz(n):
    return [
        'FizzBuzz' if x % 15 == 0 else
        'Fizz'     if x % 3  == 0 else
        'Buzz'     if x % 5  == 0 else
        x for x in range(1, n + 1)]
