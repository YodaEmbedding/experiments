import asyncio
import itertools


async def produce(queue: asyncio.Queue):
    for i in itertools.count():
        print(f"Produce: {i} <")
        await queue.put(i)
        await asyncio.sleep(0.4)


async def consume(queue: asyncio.Queue):
    while True:
        item = await queue.get()
        print(f"Consume: {item}")
        await asyncio.sleep(0.2)


def main():
    queue = asyncio.Queue()
    coros = [produce(queue), consume(queue)]
    tasks = map(asyncio.create_task, coros)
    loop = asyncio.get_event_loop()
    loop.run_until_complete(asyncio.wait(tasks))
    # loop.run_until_complete(asyncio.wait(coros))  # deprecated


main()
