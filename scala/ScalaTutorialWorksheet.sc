def factorial1(x: Int): Int =
    if (x < 1) 1 else x * factorial1(x - 1)

factorial1(4)
factorial1(0)

// def factorial2(x: Int): Int = {
//     def loop(acc: Int, x: Int) = ???
//     if (x == 0) ???
//     else ???
// }

// Tail recursive
def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int =
        if (a <= b) loop(a + 1, acc + f(a))
        else acc
    loop(a, 0)
}

sum(x => x * x, 0, 3)

// Currying
def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else product(f)(a + 1, b) * f(a)

product(x => x)(2, 4)

def factorial3(x: Int): Int = product(x => x)(1, x)

factorial3(4)

def aggregate(default: Int, combine: (Int, Int) => Int)
             (f: Int => Int)
             (a: Int, b: Int): Int =
    if (a > b) default
    else combine(aggregate(default, combine)(f)(a + 1, b), f(a))

aggregate(1, (a, b) => a * b)(x => x)(1, 4)

// TODO finish last part of Higher Order functions from Martin Odersky's course
