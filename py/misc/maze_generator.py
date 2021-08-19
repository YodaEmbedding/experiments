import itertools
from collections import deque
from enum import IntFlag
from random import randint
import numpy as np

Dir = IntFlag("Dir", ["N", "S", "W", "E"])
DirAll = Dir.N | Dir.S | Dir.W | Dir.E


class Maze:
    def __init__(self, width, height):
        self.new_maze(width, height)

    # {'┌', '╴', '┤', '│', '┐', '╷', '╵', '┬', '└', '╶', '├', '┼', ' ', '┘', '─', '┴'}
    def __str__(self):
        def vert_str(row):
            return f"+{'+'.join('--' if x else '  ' for x in row)}+"

        def horz_str(row):
            return f"{'  '.join('|' if x else ' ' for x in row)}"

        vert_rows = [vert_str(row & Dir.N) for row in self.maze] + [
            vert_str(self.maze[-1] & Dir.S)
        ]

        horz_rows = [
            horz_str(itertools.chain(row & Dir.W, [row[-1] & Dir.E]))
            for row in self.maze
        ]

        return "\n".join(
            itertools.chain.from_iterable(
                itertools.zip_longest(vert_rows, horz_rows, fillvalue="")
            )
        )

    def __getitem__(self, key):
        x, y, d = key
        if all(x != d for x in (Dir.N, Dir.W, Dir.S, Dir.E)):
            raise ValueError
        return (
            self.maze[y, x] & d != 0
            if d == Dir.N or d == Dir.W
            else self.maze[y, x + 1] & Dir.W != 0
            if d == Dir.E
            else self.maze[y + 1, x] & Dir.N != 0
            if d == Dir.S
            else None
        )

    def build_wall(self, x, y, direction):
        if direction & Dir.N:
            self.maze[y, x] |= Dir.N
        if direction & Dir.W:
            self.maze[y, x] |= Dir.W
        if direction & Dir.S:
            self.maze[y + 1, x] |= Dir.N
        if direction & Dir.E:
            self.maze[y, x + 1] |= Dir.W

    def clear_wall(self, x, y, direction):
        if direction & Dir.N:
            self.maze[y, x] &= ~Dir.N
        if direction & Dir.W:
            self.maze[y, x] &= ~Dir.W
        if direction & Dir.S:
            self.maze[y + 1, x] &= ~Dir.N
        if direction & Dir.E:
            self.maze[y, x + 1] &= ~Dir.W

    def new_maze_dfs(self, width, height):
        x, y = 0, 0
        visited = np.zeros_like(self.maze, dtype=np.bool)
        track = deque(((x, y),))
        while len(track) != 0:
            visited[y, x] = True
            choices = (
                ([Dir.N] if y > 0 and not visited[y - 1, x] else [])
                + ([Dir.W] if x > 0 and not visited[y, x - 1] else [])
                + ([Dir.S] if y < height - 1 and not visited[y + 1, x] else [])
                + ([Dir.E] if x < width - 1 and not visited[y, x + 1] else [])
            )
            if len(choices) == 0:
                x, y = track.pop()
                continue
            direction = choices[randint(0, len(choices) - 1)]
            self.clear_wall(x, y, direction)
            print(choices)
            print((x, y), direction)
            print(self)
            x += -1 if direction == Dir.W else 1 if direction == Dir.E else 0
            y += -1 if direction == Dir.N else 1 if direction == Dir.S else 0
            track.append((x, y))

    # TODO my awesome maze algorithm: random seed points, dfs, then join together?

    def new_maze(self, width, height):
        self.maze = np.full((height, width), DirAll, dtype=np.uint8)
        self.new_maze_dfs(width, height)


maze = Maze(10, 5)
# maze.clear_wall(1, 2, Dir.N)
# print(maze[(1, 2), Dir.N])
print(maze)
