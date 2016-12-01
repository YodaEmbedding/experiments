# http://math.stackexchange.com/questions/2037059/find-all-possible-integer-dimension-equiareas-inside-a-fixed-rectangle

import itertools
from collections import namedtuple

Block = namedtuple("Block", "width height")
Position = namedtuple("Position", "x y")
Shape = namedtuple("Shape", "position idx")
Interval = namedtuple("Interval", "min max")

def intersection(interval1: Interval, interval2: Interval):
	intersection = Interval(max(interval1.min, interval2.min), min(interval1.max, interval2.max))
	return None if intersection.max < intersection.min else intersection

def left(shape: Shape):
	return shape.position.x

def top(shape: Shape):
	return shape.position.y

def right(shape: Shape):
	return shape.position.x + blocks[shape.idx].width - 1

def bottom(shape: Shape):
	return shape.position.y + blocks[shape.idx].height - 1

def check_overlap(shape1: Shape, shape2: Shape):
	return (shape1.position.x <= right(shape2) and
		right(shape1) >= shape2.position.x and
		shape1.position.y <= bottom(shape2) and
		bottom(shape1) >= shape2.position.y)

def contained(shape_top: Shape, shape_bottom: Shape):
	# if top(shape_bottom) != bottom(shape_top) + 1 or (right(shape_bottom) < left(shape) and left(shape_bottom) > right(shape)):
	return None if bottom(shape_top) + 1 != top(shape_bottom) else intersection(
		Interval(left(shape_top), right(shape_top)),
		Interval(left(shape_bottom), right(shape_bottom)))

def get_divisors(num: int):
	return [i for i in range(1, num + 1) if num % i == 0]

# Returns factored pairs
# Reduces subject to constraints
def get_blocks(width: int, height: int, area: int):
	return [Block(i, area // i) for i in get_divisors(area) if
		i <= width and (area // i) <= height]

#!todo rename blocks to shapes? or vice versa?
def get_topologies(width: int, height: int, divisions: int):
	block_area = width * height // divisions
	blocks = get_blocks(width, height, block_area)

	topologies = []
	stack = [] # Current topology

	origin = Position(0, 0)

	# Check for intervals covered by the bottom edge
	def covered(shape):
		intervals = filter(lambda x: x, [contained(shape, s) for s in stack])
		sorted_intervals = sorted(intervals, key=lambda x: x.min)
		zipped = zip(sorted_intervals, sorted_intervals[1::])

		joined_intervals = [(if ___) for a, b in zipped] ???

		return joined_intervals

	# Check for intervals uncovered by the bottom edge
	def uncovered_leftmost(shape):
		return ?? 

	# Topmost, Leftest unfilled position
	def next_position():
		# Some form of tracker for how far right you've gone (over the edge?) which then forces a "recalc" of sorts...?
		# No, doesn't work too well...
		# Best to just do a filter search twice on bottom positions then on right positions

		if len(stack) == 0:
			return origin

		positions = [uncovered(s) for s in shapes if uncovered(s)]
		# flatten
		# return minimimum of the position
		???

		# NOT EXACTLY... this will always return some constant value on the top row...
		# Check if below doesn't have ...
		# get all objects with min...

		# be very careful with next filter
		# consider if rows are perfect height equal but one has already filled left and does not continuously form a vertical boundary...

		return origin if len(stack) == 0 else origin

	# Checks whether shape exceeds boundaries or overlaps with existing shapes in the stack
	def verify_fit(shape):
		return (right(shape) < width and bottom(shape) < height and
			all([check_overlap(shape, s) for s in stack]))

	# Conditions in case not enough blocks and what not... meh invalid areas...? widths height even division non integer area?

	# Recursion might be cleaner...
	# for i, block in enumerate(blocks):
	# 	stack = [Shape(origin, i)]

	j = 0

	# Loop conditions are a little too "clever"
	while j < len(blocks) and len(stack) > 0:
		if j >= len(blocks):
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
print(get_topologies(30, 16, 12))


