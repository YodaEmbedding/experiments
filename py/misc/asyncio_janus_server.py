import asyncio
import time

import janus


async def produce(reader, queue):
    while True:
        item = await reader.readline()
        if len(item) == 0:
            break
        print(f"Produce: {item}")
        await queue.put(item)
    await queue.put(None)


async def consume(writer, queue):
    while True:
        item = await queue.get()
        if item is None:
            break
        print(f"Consume: {item}")
        writer.write(item)
        await writer.drain()

    print("Closing client...")
    writer.close()


def processor(input_queue, output_queue):
    print("Starting processor...")
    while True:
        item = input_queue.get()
        result = item.upper()
        time.sleep(1.0)
        output_queue.put(result)


async def handle_client(reader, writer):
    print("New client...")
    input_queue = janus.Queue()
    output_queue = janus.Queue()

    loop = asyncio.get_running_loop()
    future = loop.run_in_executor(
        None, processor, input_queue.sync_q, output_queue.sync_q
    )

    coros = [
        produce(reader, input_queue.async_q),
        consume(writer, output_queue.async_q),
    ]
    tasks = map(asyncio.create_task, coros)
    await asyncio.wait(tasks)


async def main():
    server = await asyncio.start_server(handle_client, "localhost", 5678)
    await server.serve_forever()


asyncio.run(main())
