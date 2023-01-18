# http://math.stackexchange.com/questions/2037059/find-all-possible-integer-dimension-equiareas-inside-a-fixed-rectangle

import itertools
from collections import namedtuple
from typing import List

Block = namedtuple("Block", "width height")
Position = namedtuple("Position", "x y")
Shape = namedtuple("Shape", "position idx")
Interval = namedtuple("Interval", "min max")


def intersection(interval1: Interval, interval2: Interval):
    intersection = Interval(
        max(interval1.min, interval2.min), min(interval1.max, interval2.max)
    )
    return None if intersection.max < intersection.min else intersection


# Sorts and joins adjacent intervals
def join_intervals(intervals: List[Interval]):
    if len(intervals) < 2:
        return intervals

    joined_intervals = []
    sorted_intervals = sorted(intervals, key=lambda x: x.min)
    accumulator = sorted_intervals[0]

    def joinable(interval1, interval2):
        return interval1.max + 1 == interval2.min

    for interval in sorted_intervals[1:]:
        if joinable(accumulator, interval):
            accumulator = Interval(accumulator.min, interval.max)
        else:
            joined_intervals.append(accumulator)
            accumulator = interval

    joined_intervals.append(accumulator)

    return joined_intervals


# Complement a list of intervals inside a given closed interval domain
def complement(intervals: List[Interval], domain: Interval):
    if len(intervals) == 0:
        return domain

    joined_intervals = join_intervals(intervals)
    complemented = []

    if domain.min < joined_intervals[0].min:
        complemented.append(Interval(domain.min, joined_intervals[0].min - 1))

    if len(intervals) > 1:
        complemented.extend(
            [
                Interval(a.max + 1, b.min - 1)
                for a, b in zip(joined_intervals, joined_intervals[1:])
            ]
        )

    if domain.max > joined_intervals[-1].max:
        complemented.append(Interval(joined_intervals[0].max + 1, domain.max))

    return complemented


def get_divisors(num: int):
    return [i for i in range(1, num + 1) if num % i == 0]


# Returns factored pairs
# Reduces subject to constraints
def get_blocks(width: int, height: int, area: int):
    return [
        Block(i, area // i)
        for i in get_divisors(area)
        if i <= width and (area // i) <= height
    ]


#!todo rename blocks to shapes? or vice versa?
def get_topologies(width: int, height: int, divisions: int):
    block_area = width * height // divisions
    blocks = get_blocks(width, height, block_area)

    topologies = []
    stack = []  # Current topology

    origin = Position(0, 0)

    def left(shape: Shape):
        return shape.position.x

    def top(shape: Shape):
        return shape.position.y

    def right(shape: Shape):
        return shape.position.x + blocks[shape.idx].width - 1

    def bottom(shape: Shape):
        return shape.position.y + blocks[shape.idx].height - 1

    def check_overlap(shape1: Shape, shape2: Shape):
        return (
            shape1.position.x <= right(shape2)
            and right(shape1) >= shape2.position.x
            and shape1.position.y <= bottom(shape2)
            and bottom(shape1) >= shape2.position.y
        )

    def contained(shape_top: Shape, shape_bottom: Shape):
        # if top(shape_bottom) != bottom(shape_top) + 1 or (right(shape_bottom) < left(shape) and left(shape_bottom) > right(shape)):
        return (
            None
            if bottom(shape_top) + 1 != top(shape_bottom)
            else intersection(
                Interval(left(shape_top), right(shape_top)),
                Interval(left(shape_bottom), right(shape_bottom)),
            )
        )

    # Check for intervals covered by the bottom edge
    def covered(shape: Shape):
        return join_intervals(
            list(filter(lambda x: x, [contained(shape, s) for s in stack]))
        )

    # Check for intervals uncovered by the bottom edge
    def uncovered(shape: Shape):
        return complement(covered(shape), Interval(left(shape), right(shape)))

    # This is pretty damn difficult...
    # try different ordering, maybe...??
    # or method to obtain coordinates?
    # faster one too...

    # Topmost, Leftest unfilled position
    def next_position():
        # Some form of tracker for how far right you've gone (over the edge?) which then forces a "recalc" of sorts...?
        # No, doesn't work too well...
        # Best to just do a filter search twice on bottom positions then on right positions

        if len(stack) == 0:
            return origin

        #!todo
        # Forgot to consider if top row is not yet filled!?!
        # Possible workarounds:
        # add in a rectangle on the stack at position -1

        # If top row not filled, return rightmost
        if max(stack, key=lambda s: top(s)) == 0:
            x = max((right(s) for s in stack), key=lambda s: right(s) + 1)
            if x < width:
                return Position(x, 0)

        #!todo verify correctness... top(s)?? wat??? stack???
        positions = [
            Position(uncovered(s)[0].min, top(s))
            for s in stack
            if uncovered(s)
        ]
        left_sort = sorted(positions, key=lambda p: p.x)
        top_sort = sorted(left_sort, key=lambda p: p.y)

        return Position(top_sort[0].x, top_sort[0].y)

    # Checks whether shape exceeds boundaries or overlaps with existing shapes in the stack
    def verify_fit(shape: Shape):
        return (
            right(shape) < width
            and bottom(shape) < height
            and all([check_overlap(shape, s) for s in stack])
        )

    # Conditions in case not enough blocks and what not... meh invalid areas...? widths height even division non integer area?

    # Recursion might be cleaner...
    # for i, block in enumerate(blocks):
    # 	stack = [Shape(origin, i)]

    j = 0

    # Loop conditions are a little too "clever"
    while j < len(blocks):
        if j >= len(blocks):
            if len(stack) == 0:
                break
            j = stack.pop().idx + 1
            continue

        # Append successful topologies
        if len(stack) == divisions:
            topologies.append(list(stack))
            j = stack.pop().idx + 1
            continue

        pos = next_position()
        shape = Shape(pos, j)

        if verify_fit(shape):
            stack.append(shape)
            j = 0
            continue

        j = j + 1

    return topologies


print(get_blocks(30, 16, 40))
# print(get_topologies(30, 16, 12))
print(get_topologies(16, 30, 12))
