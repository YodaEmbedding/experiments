# http://stackoverflow.com/questions/40499893/creating-new-lines-between-ascii-encoded-strings

data = """36 30 25 40 80 4
37 30 25 40 80 4
40 30 25 40 80 4"""

lines = [x for x in data.split("\n")]
nums_list = [[int(c) for c in x.split(" ")] for x in lines]
char_list = [[chr(sum(nums[i:i+2])) for i in range(0, len(nums), 2)] for nums in nums_list]
word_list = [''.join(c) for c in char_list]
print(word_list)

print(" ".join(word_list))
