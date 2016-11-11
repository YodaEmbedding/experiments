# https://www.youtube.com/watch?v=OSGv2VnC0go

names = ['rouge', 'geralt', 'blizzard', 'yennefer']
colors = ['red', 'green', 'blue', 'yellow']

for color in reversed(colors):
	print(color)

for i, color in enumerate(colors):
	print(i, '-->', color)

for name, color in zip(names, colors):
	print('{:<12}{:<12}'.format(name, color))

for color in sorted(zip(names, colors), key=lambda x: x[0]):
	print(color)
