#!/usr/bin/env python3

from random import randint


def monty(n_doors):
    car = randint(0, n_doors - 1)
    first_choice = 0  # always pick 0 for convenience
    remaining_door = car if first_choice != car else 1  # 1 for convenience
    return remaining_door == car


total_runs = 1000000
trials = [monty(3) for x in range(total_runs)]

print(sum(trials) / total_runs)
