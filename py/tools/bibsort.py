"""Sorts a reasonably well formatted .bib file.

For example, the following .bib file:

```bibtex
@misc{bob,
    ...
}

@misc{alice,
    ...
}

```

becomes:

```bibtex
@misc{alice,
    ...
}

@misc{bob,
    ...
}

```
"""

import sys

filenames = sys.argv[1:]

for filename in filenames:
    entries = []

    # Parse file. Assume reasonably well-formatted.
    with open(filename) as f:
        for line in f:
            if line.startswith("@"):
                entries.append([])
            entries[-1].append(line)

    # Sort by "key" in "@misc{key,".
    entries.sort(key=lambda lines: lines[0].split("{")[1].lower())

    # Save sorted file.
    with open(filename, "w") as f:
        f.write("".join(x for xs in entries for x in xs))
