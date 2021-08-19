from threading import current_thread
from time import sleep

import rx
import rx.subject
from rx import operators as ops
from rx.scheduler import ThreadPoolScheduler


def print_thread(tag):
    def inner(x):
        print(tag, x, current_thread().name)

    return inner


def write(x):
    log = print_thread("writer")
    log(x)
    sleep(0.4)


def read():
    for i in range(10):
        yield i
        sleep(1)


def main():
    pool_scheduler = ThreadPoolScheduler(2)
    writer = rx.subject.Subject()
    reader = rx.from_iterable(read())

    # Subjects behave differently than Observables w.r.t. subscribe_on
    # subscribe_on doesn't work since on_next() is called on original thread
    # observe_on ensures we don't block the thread calling on_next()
    writer.pipe(
        ops.observe_on(pool_scheduler),
    ).subscribe(write)

    reader.subscribe(print_thread("reader"), scheduler=pool_scheduler)

    for i in range(10):
        writer.on_next(i)
        sleep(1)


main()
