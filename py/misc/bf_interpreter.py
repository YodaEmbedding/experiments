# Interprets the BrainFuck programming language

def execute(program_text: str, input_text: str):
	output_chars = []
	bracket_stack = []
	stack = [0]
	stack_index = 0
	input_index = 0
	program_index = 0

	def get_matching_bracket_end():
		search_index = program_index + 1
		bracket_count = 1
		
		while bracket_count > 0:
			if program_text[search_index] == '[':
				bracket_count += 1
			elif program_text[search_index] == ']':
				bracket_count -= 1
			search_index += 1

		return search_index

	while program_index < len(program_text):
		c = program_text[program_index]

		if c == '+':
			stack[stack_index] += 1
		elif c == '-':
			stack[stack_index] -= 1
		elif c == '>':
			stack_index += 1
			if stack_index == len(stack):
				stack.append(0)
		elif c == '<':
			stack_index -= 1
			if stack_index < 0:
				raise IndexError("Stack cannot be indexed at negative locations")
		elif c == '.':
			output_chars.append(chr(stack[stack_index]))
		elif c == ',':
			stack[stack_index] = ord(input_text[input_index])
			input_index += 1
		elif c == '[':
			if stack[stack_index] == 0:
				program_index = get_matching_bracket_end()
				continue
			else:
				bracket_stack.append(program_index)
		elif c == ']':
			program_index = bracket_stack.pop()
			continue

		program_index += 1

	return ''.join(output_chars)


# Example 1

output_text = execute("""
	++++++++			loop 8 times
	[
		>+++++++++		increment by 9
		<-
	]
	>.					print H = 8 * 9
	,.					print i
	,.					print !
""", "i!")

print(output_text)

# Example 2

output_text = execute("""
	+++					loop 3 times
	[
		>++++			loop 4 times
		[
			>++++++		increment by 6
			<-
		]
		<-
	]
	>>.					print H = 3 * 4 * 6
	+.					print I = 3 * 4 * 6 + 1
	,.					print !
	,.					print space
	,.					print :
	,.					print )
""", "! :)")

print(output_text)
