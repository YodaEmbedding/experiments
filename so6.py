def get_divisors(num):
	return [i for i in range(1, num) if num % i == 0]

def sum_divisors(num):
	return sum(get_divisors(num))

def is_finite(num):
	previous = []

	while num != 0:
		if num in previous:
			return False

		previous.append(num)
		num = sum_divisors(num)

	return True

print(is_finite(6))