from sympy import *
from IPython.display import display
from IPython.display import Latex

# init_session()
# init_printing(use_latex=True)

x, y, z = symbols('x y z')

a = Integral(cos(x), x)
b = Integral(cos(x), (x, 0, 2*pi))

print(a, "=", a.doit())
display(Eq(a, a.doit()))

print(b, '=', b.doit())
display(Eq(b, b.doit()))

print(latex(b))

