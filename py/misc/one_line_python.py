# Useful functions:
id = lambda x: x

# Multiple prints:
# print(1)
# print(2)
# print(3)
((lambda *args: None)
 (print(1),
  print(2),
  print(3)))

# Stateful-expressions
# x = 4
# print(x)
# x = x - 2
# print(x)
((lambda x: (x, print(x))[0])
 ((lambda x: x - 2)
  ((lambda x: (x, print(x))[0])
   ((lambda x: 4)
    (None)))))

# Alternatively:
lift = lambda f: lambda x: (x, f(x))[0]  # lift f s.t. its computation is ignored
f1 = lambda x: 4
f2 = lift(print)
f3 = lambda x: x - 2
f4 = lift(print)
f4(f3(f2(f1(None))))

# You can simply substitute all of this into one line trivially:
f4(f3(f2((lambda x: 4)(None))))
f4(f3(lift(print)((lambda x: 4)(None))))
f4((lambda x: x - 2)(lift(print)((lambda x: 4)(None))))
lift(print)((lambda x: x - 2)(lift(print)((lambda x: 4)(None))))

# Reformatting a bit:
(lift(print)
 ((lambda x: x - 2)
  (lift(print)
   ((lambda x: 4)
    (None)))))

# Substituting lift:
((lambda f: lambda x: (x, f(x))[0])(print)
 ((lambda x: x - 2)
  ((lambda f: lambda x: (x, f(x))[0])(print)
   ((lambda x: 4)
    (None)))))

# And this can of course be reduced to our very first example:
((lambda x: (x, print(x))[0])
 ((lambda x: x - 2)
  ((lambda x: (x, print(x))[0])
   ((lambda x: 4)
    (None)))))

# It should be noted that we can define lift in a manner without using tuples
# by using a function that ignores the second argument
const = lambda x: lambda y: x
lift = lambda f: lambda x: const(x)(f(x))
(lift(print)
 ((lambda x: x - 2)
  (lift(print)
   ((lambda x: 4)
    (None)))))

# ...just in case you thought the tuple was actually necessary :)
# Actually, you can define numbers and logical operations
# entirely in terms of lambda functions (see Church encodings)

# So far, we appear to be writing our programs backwards.
# If a more imperative ordering of these functions is desired:
def bind(x, f):
    return lambda g: bind(f(x), g)

bind(None, f1)(f2)(f3)(f4)(id)

# Where bind can be written in lambda form:
# bind = lambda x, f:
# bind = (lambda f: f(f))(lambda x, f: )

# Recursive functions (e.g. fibonacci)

# Fizz buzz

# Y-combinator
Y = (lambda f: (lambda x: f(x(x)))(lambda x: f(x(x))))

