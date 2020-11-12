import time

import rx
from rx import operators as ops
from rx.subject import Subject

# numbers = rx.from_iterable(i for i in range(10)).pipe(
#     ops.publish(),
# )
#
# numbers.pipe(
#     ops.filter(lambda x: x % 2 == 0)
# ).subscribe(lambda x: print(f"Even:   {x}"))
#
# numbers.pipe(
#     ops.filter(lambda x: x % 2 == 1)
# ).subscribe(lambda x: print(f"Odd:    {x}"))
#
# numbers.pipe(
#     ops.filter(lambda x: x % 3 == 0)
# ).subscribe(lambda x: print(f"Triple: {x}"))
#
# zipping = numbers.pipe(
#     ops.zip(numbers),
# )
# zipping.subscribe(lambda x: print(x))
#
# numbers.subscribe(lambda x: print("---------"))
# numbers.connect()

frames = rx.interval(1)

# simulate n seconds of computation?
class Module:
    def to_rx(self, *inputs):
        if len(inputs) == 0:
            raise ValueError
        if len(inputs) == 1:
            observable = inputs[0]
        else:
            observable = rx.zip(*inputs)
        return observable
    # return observable.pipe(ops.map(), ops.publish())
    # result.connect

# frames.subscribe(print)

module = Module()
module

# ...

# gate = Subject()
# gate.subscribe(lambda x: print(f"received {x}"))
# rx.interval(1).pipe(
#     ops.do_action(lambda x: gate.on_next(f"gate: {x}")),
# ).subscribe()

time.sleep(10)

# As long as we are always using CPU, the thread ordering doesn't really matter
