import asyncio
import concurrent.futures
import threading
from contextlib import contextmanager, suppress
from typing import Any, Coroutine, TypeVar

T = TypeVar("T")


async def make_coro():
    try:
        while True:
            print("Running make_coro")
            # raise ValueError("This is an error")
            await asyncio.sleep(5)
    except asyncio.CancelledError:
        print("Cancelled make_coro")
        raise


@contextmanager
def new_threaded_asyncio_event_loop():
    """Creates a new asyncio event loop in a new thread."""
    loop = asyncio.new_event_loop()
    thread = threading.Thread(target=loop.run_forever)
    thread.start()
    try:
        yield loop
    finally:
        loop.call_soon_threadsafe(loop.stop)
        thread.join()


async def run_until_stop(coro: Coroutine[Any, Any, T], stop: asyncio.Event) -> T:
    """Runs a coroutine until the stop event is set."""
    task = asyncio.create_task(coro)
    task_stopper = asyncio.create_task(stop.wait())
    await asyncio.wait((task, task_stopper), return_when=asyncio.FIRST_COMPLETED)
    task.cancel()
    task_stopper.cancel()
    return await task


def main():
    stop = asyncio.Event()

    with new_threaded_asyncio_event_loop() as loop:
        future = asyncio.run_coroutine_threadsafe(
            run_until_stop(make_coro(), stop), loop
        )

        try:
            future.result()  # Wait for the future to finish.

        except KeyboardInterrupt:
            loop.call_soon_threadsafe(stop.set)

            with suppress(concurrent.futures.CancelledError):
                future.result()  # Wait for the future to finish.


if __name__ == "__main__":
    main()


# def create_event_loop_in_thread():
#     loop = asyncio.new_event_loop()
#     thread = threading.Thread(target=loop.run_forever)
#     thread.start()
#     return loop, thread
#
# async def run_until_stop(coro, stop):
#     async def cancel_on_stop():
#         await stop.wait()
#         task.cancel()
#
#     task = asyncio.create_task(coro)
#     stop_task = asyncio.create_task(cancel_on_stop())
#     result = await task
#     stop_task.cancel()
#     await stop_task
#     return result
#
# async def run_until_stop(coro, stop):
#     task = asyncio.create_task(coro)
#     asyncio.create_task(stop.wait()).add_done_callback(lambda _: task.cancel())
#     await task
