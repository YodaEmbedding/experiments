# https://krastanov.wordpress.com/2012/06/30/part-1-what-is-a-tensor-and-how-is-it-implemented-in-the-diffgeom-sympy-module/

import numpy
import scipy
from sympy.diffgeom import Manifold, Patch, CoordSystem


m = Manifold('my_manifold', 2) # A 2D manifold called 'my_manifold'
p = Patch('my_patch', m) # A patch called 'my_patch'

cs_r = CoordSystem('R', p) # A coordinate system called 'R' (for rectangular)
point = cs_r.point([1,1]) # A point with coordinates (1, 1)

print(point)




