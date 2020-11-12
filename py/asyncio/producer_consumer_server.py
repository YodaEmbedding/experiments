import asyncio


async def produce(reader, queue: asyncio.Queue):
    while True:
        item = await reader.readline()
        if len(item) == 0:
            break
        print(f"Produce: {item}")
        await queue.put(item)
    await queue.put(None)


async def consume(writer, queue: asyncio.Queue):
    while True:
        item = await queue.get()
        if item is None:
            break
        print(f"Consume: {item}")
        writer.write(item)
        await writer.drain()

    print("Closing client...")
    writer.close()


async def handle_client(reader, writer):
    print("New client...")
    queue = asyncio.Queue()
    coros = [produce(reader, queue), consume(writer, queue)]
    tasks = map(asyncio.create_task, coros)
    await asyncio.wait(tasks)


async def main():
    server = await asyncio.start_server(handle_client, "localhost", 5678)
    await server.serve_forever()


asyncio.run(main())
