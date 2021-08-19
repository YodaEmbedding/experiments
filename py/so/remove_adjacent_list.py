import re
from collections import defaultdict


def open_tag_as_str(tag):
    m = re.match(r"^<(\w+)>$", tag)
    return None if m is None else m.group(1)


def close_tag_as_str(tag):
    m = re.match(r"^</(\w+)>$", tag)
    return None if m is None else m.group(1)


def remove_adjacent_tags(tags):
    def closes(a, b):
        a = open_tag_as_str(a)
        b = close_tag_as_str(b)
        return a is not None and b is not None and a == b

    # This is a bit ugly and could probably be improved with
    # some itertools magic or something
    skip = False
    for i in range(len(tags)):
        if skip:
            skip = False
        elif i + 1 < len(tags) and closes(tags[i], tags[i + 1]):
            skip = True
        else:
            yield tags[i]


# # Nevermind... this function does not meet up to OP's strange requirements
# def remove_adjacent_tags(tags):
#     def closes(a, b):
#         a = open_tag_as_str(a)
#         b = close_tag_as_str(b)
#         return a is not None and b is not None and a == b
#
#     open_stack = defaultdict(list)
#     close_stack = defaultdict(list)
#
#     for i, tag in enumerate(tags):
#         open_stack[open_tag_as_str(tag)].append(i)
#         close_stack[close_tag_as_str(tag)].append(i)
#
#     open_stack[None] = []
#     close_stack[None] = []
#
#     for i, tag in enumerate(tags):
#         o = open_tag_as_str(tag)
#         c = close_tag_as_str(tag)
#
#         if o is not None:
#             if len(close_stack[o]) == 0:
#                 yield tag
#             else:
#                 close_stack[]
#         elif c is not None and len(open_stack[c]) == 0:
#             yield tag
#             ...
#         else:
#             yield tag
#             ...

boo = ["<a>", "<b>", "<c>", "</c>", "</b>", "</a>"]
boo = list(remove_adjacent_tags(boo))
print(boo)
