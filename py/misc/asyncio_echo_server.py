import asyncio

async def handle_client(reader, writer):
    print("New client...")

    while True:
        request = await reader.readline()
        if len(request) == 0:
            break
        print(f"Received: {request}")
        response = request
        writer.write(response)
        await writer.drain()

    print("Closing client...")
    writer.close()

loop = asyncio.get_event_loop()
loop.create_task(asyncio.start_server(handle_client, 'localhost', 5678))
loop.run_forever()
