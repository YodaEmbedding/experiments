from time import sleep
from typing import *

import rx
import rx.operators as ops


def zip_latest(*xss: rx.Observable) -> rx.Observable:
    helper = ZipLatestHelper(len(xss))
    return mux(*xss).pipe(
        ops.map(helper.process),
        ops.filter(lambda x: x is not None),
    )


def mux(*xss: rx.Observable) -> rx.Observable:
    def pair_index(i: int) -> Callable[[Any], Tuple[int, Any]]:
        def inner(x: Any) -> Tuple[int, Any]:
            return i, x

        return inner

    paired = [xs.pipe(ops.map(pair_index(i))) for i, xs in enumerate(xss)]
    return rx.from_iterable(paired).pipe(ops.merge_all())


class ZipLatestHelper:
    def __init__(self, num_streams):
        self.latest = [None for _ in range(num_streams)]
        self.ready = set()

    def process(self, pair: Tuple[int, Any]) -> Optional[Tuple[Any, ...]]:
        i, x = pair
        self.latest[i] = x
        self.ready.add(i)
        return (
            tuple(self.latest) if len(self.ready) == len(self.latest) else None
        )


zipped = zip_latest(
    rx.interval(0.5).pipe(ops.map(lambda i: f"A{i}")),
    rx.interval(0.3).pipe(ops.map(lambda i: f"B{i}")),
)

zipped.subscribe(print)

sleep(10)
