# Useful functions:
id = lambda x: x
const = lambda x: lambda y: x

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

# Substituting f1, f2, f3, f4 gives:
(lift(print)
 ((lambda x: x - 2)
  (lift(print)
   ((lambda x: 4)
    (None)))))

# Substituting lift gives:
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

# Or with a bit of currying:
def bind_curry(x):
    return lambda f: bind(x, f)

bind_curry(None)(f1)(f2)(f3)(f4)(id)

# Which gives the expression a more traditional, imperative ordering:
(bind_curry(None)
 (lambda x: 4)
 (lift(print))
 (lambda x: x - 2)
 (lift(print))
 (id))

# Note that although bind is recursive, it can still be written in lambda form:
# bind = lambda x, f:
# bind = (lambda f: f(f))(lambda x, f: )
# bind()
# bind = (lambda f: )
# TODO

# Recursive functions
def factorial(n):
    return n * factorial(n - 1) if n > 1 else 1

# Let's make this tail recursive:
def factorial(n):
    def helper(n, acc):
        return helper(n - 1, acc * n) if n > 1 else acc
    return helper(n, 1)

# Now let's translate it to lambda form:
helper = lambda n, acc: helper(n - 1, acc * n) if n > 1 else acc
factorial = lambda n: helper(n, 1)

print(factorial(5))

# But how can we combine that into one line?
# (lambda f, x: f(x(x)))
# (lambda f, x: f(f, x))(f)


# (lambda f, n, x: f(f, n - 1, x * n) if x > 1 else x)(lambda x

# Y-combinator
Y = (lambda f: (lambda g: f(g(g)))(lambda g: f(g(g))))


# Fizz buzz

