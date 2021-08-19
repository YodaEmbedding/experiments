import asyncio
import random
import time


async def io_bound(x):
    print(f"Task {x} started")
    dt = random.uniform(0.1, 1.0)
    t = time.time()
    await asyncio.sleep(dt)
    elapsed = time.time() - t
    print(f"Task {x} await took {elapsed:.6f} versus expected {dt:.6f}")
    return x


def create_coroutines(n=3):
    return [io_bound(x) for x in range(n)]


# class Collector:
#     def __init__(self, tasks):
#         self.tasks = tasks
#
#     async def __anext__(self):


async def main():
    tasks = [asyncio.create_task(x) for x in create_coroutines()]

    # Guaranteed processing order
    for task in tasks:
        x = await task
        print(f"Processed {x}")

    print("")

    tasks = [asyncio.create_task(x) for x in create_coroutines()]

    # Guaranteed processing order
    # async for task in tasks:
    #     x = await task
    #     print(f'Processed {x}')

    # for result in await asyncio.gather(*tasks):
    #     print(result)

    results = await asyncio.gather(*tasks)
    print(results)


asyncio.run(main())
