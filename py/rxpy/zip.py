import time

import rx
from rx import operators as ops

t = 0.01

a = rx.interval(t)
b = rx.interval(10 * t)

a.pipe(
    ops.map(lambda x: 1 / 0),
    ops.zip(b),
).subscribe(print)

time.sleep(1000 * t)
