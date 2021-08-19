# https://stackoverflow.com/questions/51054475/is-there-a-better-more-pythonic-method-to-print-columns-based-on-dictionary-keys

import itertools

menu_height = 8
items = [chr(ord("a") + x) for x in range(21)]

menu_dict = {x: "String for " + x for x in items}
menu_dict[None] = ""


def format_item(x):
    if x == None:
        return " " * 37
    return "[%s] %-33s" % (x, menu_dict[x])


h = menu_height
cols = itertools.izip_longest(
    items[0:h], items[h : 2 * h], items[2 * h :], fillvalue=None
)

lines = (" ".join(format_item(x) for x in t) for t in cols)
print("\n".join(lines))
