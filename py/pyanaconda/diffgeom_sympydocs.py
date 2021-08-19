# http://docs.sympy.org/latest/modules/diffgeom.html

from sympy import symbols, sin, cos, pi
from sympy.diffgeom import Manifold, Patch, CoordSystem
from sympy.simplify import simplify

r, theta = symbols("r, theta")

m = Manifold("M", 2)
patch = Patch("P", m)

rect = CoordSystem("rect", patch)
polar = CoordSystem("polar", patch)

print(rect in patch.coord_systems)

polar.connect_to(rect, [r, theta], [r * cos(theta), r * sin(theta)])

print(polar.coord_tuple_transform_to(rect, [0, 2]))
print(polar.coord_tuple_transform_to(rect, [2, pi / 2]))
print(rect.coord_tuple_transform_to(polar, [1, 1]).applyfunc(simplify))

print(polar.jacobian(rect, [r, theta]))

p = polar.point([1, 3 * pi / 4])
print(rect.point_to_coords(p))
print(rect.coord_function(0)(p))
print(rect.coord_function(1)(p))

v_x = rect.base_vector(0)
x = rect.coord_function(0)
print(v_x(x))
print(v_x(v_x(x)))

v_r = polar.base_vector(0)
print(v_r(x))
# ^ ????

dx = rect.base_oneform(0)
print(dx(v_x))
# ????

# Skipped some stuff...


# Tensor products
# Christoffel Symbols
