import multiprocessing
from threading import current_thread

import rx
from rx import operators as ops
from rx.scheduler import CurrentThreadScheduler, ThreadPoolScheduler


def print_thread(tag):
    def inner(x):
        print(tag, x, current_thread().name)

    return inner


pool_scheduler = ThreadPoolScheduler(multiprocessing.cpu_count())

rx.from_iterable(range(10)).pipe(
    # Runs on main thread
    ops.do_action(print_thread(0)),
    # Switch threads to unused one from pool
    ops.flat_map(
        lambda x: rx.just(x, pool_scheduler).pipe(
            ops.do_action(print_thread(1)),
        )
    ),
    # This executes on the same thread from the pool as in the flat_map
    ops.do_action(print_thread(2)),
).subscribe()

print("This may be printed on any line *after* the final MainThread print!")
