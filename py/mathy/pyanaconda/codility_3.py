# https://codility.com/c/feedback/MK3WA3-KF3

# from collections import namedtuple
# Vector = namedtuple('Vector', ['x', 'y'])

from math import atan2, cos, sin, sqrt


class Vector:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)

    def length(self):
        return sqrt(self.x**2 + self.y**2)

    def angle(self):
        return atan2(self.y, self.x)

    def rotate(self, angle):
        c = self.length()
        t = self.angle() + angle
        self.x = c * cos(t)
        self.y = c * sin(t)

    def __str__(self):
        return "(" + str(self.x) + ", " + str(self.y) + ")"


def solution(K, L, M, N, P, Q, R, S):
    origin = Vector(K, L)

    v = Vector(M, N) - origin
    v1 = Vector(P, Q) - origin
    v2 = Vector(R, S) - origin

    angle = v.angle()
    v.rotate(-angle)
    v1.rotate(-angle)
    v2.rotate(-angle)

    def within(p):
        return 0 <= p.x <= v.x

    def opposing(p, q):
        return (p.x <= 0 and v.x <= q.x) or (q.x <= 0 and v.x <= p.x)

    return ((v1.y >= 0 and v2.y <= 0) or (v1.y >= 0 and v2.y <= 0)) and (
        within(v1) or within(v2) or opposing(v1, v2)
    )


print(solution(0, 1, 4, 3, 1, 3, 2, 1))
print(solution(0, 1, 4, 3, 3, 2, 5, 1))
