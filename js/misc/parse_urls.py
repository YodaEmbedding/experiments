import sys

results = []

for line in sys.stdin.read().splitlines():
    line = line.strip()
    result = f'    "{line}",'
    results.append(result)
    print(result)

with open("lichessRequestAnalysis.js") as f:
    lines = f.read().splitlines()

arr_start_idx = next(i for i, x in enumerate(lines) if "gameUrls = " in x) + 1
lines = lines[:arr_start_idx] + results + lines[arr_start_idx:]

with open("lichessRequestAnalysis.js", "w") as f:
    for line in lines:
        print(line, file=f)

print(f"Games added: {len(results)}")
