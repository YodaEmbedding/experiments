import random
from math import exp

epic_rate = 10
legendary_rate = 20
steepness = 1


def logistic(t, t0, k):
    return 1 / (1 + exp(-k * (t - t0)))


def should_drop_epic(consecutive_kegs):
    return random.random() < logistic(consecutive_kegs, epic_rate, steepness)
