import rx
from rx import operators as ops

numbers = rx.from_iterable(i for i in range(10)).pipe(
    ops.publish(),
)

numbers.pipe(ops.filter(lambda x: x % 2 == 0)).subscribe(
    lambda x: print(f"Even:   {x}")
)

numbers.pipe(ops.filter(lambda x: x % 2 == 1)).subscribe(
    lambda x: print(f"Odd:    {x}")
)

numbers.pipe(ops.filter(lambda x: x % 3 == 0)).subscribe(
    lambda x: print(f"Triple: {x}")
)

numbers.subscribe(lambda x: print("---------"))
numbers.connect()
