import re
import sys


def parse_pgn(lines):
    game = {}

    for line in lines:
        line = line.rstrip("\n")

        if line == "":
            if "Moves" in game:
                yield game
                game = {}
            continue

        match = re.search(r'^\[(?P<key>\w+) "(?P<value>.*)"\]$', line)

        if match:
            game[match.group("key")] = match.group("value")
            continue

        match = re.match(r"^\s*((\d+\.)|0-1|1-0|1/2-1/2)", line)

        if match:
            if "Moves" not in game:
                game["Moves"] = ""
            game["Moves"] += f"{line}\n"
            continue

        raise Exception("Invalid line: {}".format(line))


def main():
    filename = sys.argv[1]

    with open(filename) as f:
        for game in parse_pgn(f):
            if "%eval" in game["Moves"]:
                continue
            if re.match(r".*\b(Casual|Bullet|UltraBullet)\b", game["Event"]):
                continue
            print(game["Site"])


if __name__ == "__main__":
    main()
