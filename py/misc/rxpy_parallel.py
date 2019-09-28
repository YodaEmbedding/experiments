import multiprocessing
from threading import current_thread

import rx
from rx import operators as ops
from rx.scheduler import ThreadPoolScheduler, CurrentThreadScheduler


def print_thread(tag):
    def inner(x):
        print(tag, x, current_thread().name)
    return inner

optimal_thread_count = multiprocessing.cpu_count() + 1
pool_scheduler = ThreadPoolScheduler(optimal_thread_count)
rx.from_iterable(range(10)).pipe(
    ops.do_action(print_thread(0)),
    ops.flat_map(
        lambda x: rx.just(x, pool_scheduler).pipe(
            ops.do_action(print_thread(1)),
        )
    ),
).subscribe(print_thread(2), CurrentThreadScheduler())
