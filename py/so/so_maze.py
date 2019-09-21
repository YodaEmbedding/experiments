class Cell:
    def __init__(self, walls):
        self.walls = walls

    def __str__(self):
        """Allows us to call print() on a Cell!"""
        return '\n'.join(self.draw())

    def draw(self, inner='   '):
        """Draw top, mid, and bottom parts of the cell."""
        def draw_wall(wall, s):
            return s if wall in self.walls else ' ' * len(s)
        top = ' ' + draw_wall('N', '___') + ' '
        mid = draw_wall('W', '|') + inner + draw_wall('E', '|')
        bot = ' ' + draw_wall('S', '___') + ' '
        return top, mid, bot

    def can_move(self, direction):
        return direction not in self.walls

class Maze:
    def __init__(self, width, height, rows):
        self.width = width
        self.height = height
        self.rows = rows
        self.position = (0, 0)

    def __str__(self):
        return '\n'.join(self.draw_row(i) for i, _ in enumerate(self.rows))

    def _inner(self, i, j):
        return ' x ' if (i, j) == self.position else '   '

    def draw_row(self, i):
        triples = [
            cell.draw(self._inner(i, j)) for j, cell in enumerate(self.rows[i])
        ]
        return '\n'.join([
            ''.join(t for t, _, _ in triples),
            ''.join(m for _, m, _ in triples),
            ''.join(b for _, _, b in triples)])

    def cell_at_position(self, i, j):
        return self.rows[i][j]

    def move_direction(self, direction):
        curr_cell = self.cell_at_position(*self.position)
        if not curr_cell.can_move(direction):
            print("Can't go in that direction!")
            return
        deltas = {'N': (-1, 0), 'W': (0, -1), 'S': (1, 0), 'E': (0, 1)}
        y, x = self.position
        dy, dx = deltas[direction]
        self.position = y + dy, x + dx

maze = Maze(3, 3, [
    [Cell('NWE'),  Cell('NW'),   Cell('NE')],
    [Cell('WE'),   Cell('WE'),   Cell('WE')],
    [Cell('WS'),   Cell('SE'),   Cell('WSE')]
])

while True:
    print(maze)
    direction = input('What direction? ')
    maze.move_direction(direction)
