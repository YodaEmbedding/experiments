# https://www.reddit.com/r/Python/comments/9oqlvl/i_made_this_with_python_so_proud/e7x50gu/

import cairo
import math


class CelestialBody:
    def __init__(self, radius, orbit_radius, orbit_center, fill):
        self.radius = radius
        self.orbit_radius = orbit_radius
        self.orbit_center = orbit_center
        self.parent_body = None
        self.phase = 0.0
        self.fill = fill
        self.update()

    def update(self):
        if self.parent_body is not None:
            self.orbit_center = self.parent_body.center

        self.center = (
            self.orbit_center[0] + self.orbit_radius * math.cos(self.phase),
            self.orbit_center[1] + self.orbit_radius * math.sin(self.phase),
        )

    def draw(self, context):
        context.set_source_rgb(1, 1, 1)
        context.arc(*self.center, self.radius, 0, 2 * math.pi)
        if self.fill:
            context.fill()
        else:
            context.stroke()

    def bind_body(self, body):
        self.parent_body = body


def clear_screen(context):
    context.set_source_rgb(0, 0, 0)
    context.rectangle(0, 0, 1000, 1000)
    context.fill()


surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, 1000, 1000)
context = cairo.Context(surface)

FRAME_COUNT = 100

bodies = {
    "blackhole": CelestialBody(125, 0, (500, 500), False),
    "planet": CelestialBody(25, 300, (500, 500), True),
    "moon": CelestialBody(5, 50, (750, 500), False),
}

bodies["planet"].bind_body(bodies["blackhole"])
bodies["moon"].bind_body(bodies["planet"])

for i in range(FRAME_COUNT):
    clear_screen(context)

    bodies["planet"].phase = i * 2 * math.pi / FRAME_COUNT
    bodies["moon"].phase = i * 4 * math.pi / FRAME_COUNT

    for _, body in bodies.items():
        body.update()
        body.draw(context)

    context.stroke()
    surface.write_to_png(f"out/frame{i}.png")
