def box_lines(lines, width):
    topBottomRow = "+" + "-" * width + "+"
    middle = "\n".join("|" + x.ljust(width) + "|" for x in lines)
    return "{0}\n{1}\n{0}".format(topBottomRow, middle)


def split_line(line, width):
    return [line[i : i + width] for i in range(0, len(line), width)]


def split_msg(msg, width):
    lines = msg.split("\n")
    split_lines = [split_line(line, width) for line in lines]
    return [item for sublist in split_lines for item in sublist]  # flatten


def border_msg(msg, width):
    return box_lines(split_msg(msg, width), width)


print(
    border_msg(
        """I'll always remember
The chill of November
The news of the fall
The sounds in the hall
The clock on the wall
Ticking away""",
        20,
    )
)
