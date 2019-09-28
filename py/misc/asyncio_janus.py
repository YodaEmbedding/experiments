import asyncio
import itertools
import time

import janus


async def produce(queue):
    for i in itertools.count():
        print(f"Produce: {i}")
        await queue.put(i)
        await asyncio.sleep(0.4)


async def consume(queue):
    while True:
        item = await queue.get()
        print(f"Consume: {item}")


def processor(input_queue, output_queue):
    while True:
        item = input_queue.get()
        desc = "Even" if item % 2 == 0 else "Odd"
        time.sleep(1.0)
        output_queue.put(f"{item} {desc}")


def main():
    input_queue = janus.Queue()
    output_queue = janus.Queue()
    coros = [produce(input_queue.async_q), consume(output_queue.async_q)]
    tasks = map(asyncio.create_task, coros)
    loop = asyncio.get_event_loop()
    future = loop.run_in_executor(
        None, processor, input_queue.sync_q, output_queue.sync_q
    )
    loop.run_until_complete(asyncio.wait(tasks))
    loop.run_until_complete(future)


main()
