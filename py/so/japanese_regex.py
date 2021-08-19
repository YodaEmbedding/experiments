import re

s = """{おもしろい|ADJ}{本|N}を読んだ
このまえ{あたらしい|ADJ}{雑誌|N}を{いくつか|QUANT}買った
{SF|GEN}の{本|N}を読んだ"""

s = s.replace("}{", "} {")

r = r"\{([^|]+)" + r"\|" + r"[A-Z]+\}"

s = re.sub(r, r"\1", s)

print(s)
