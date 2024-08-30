import asyncio


async def long_worker():
    try:
        print("Sleeping for 1 minute...")
        await asyncio.sleep(60)
    except asyncio.CancelledError:
        print("CancelledError raised during await asyncio.sleep")
        raise
    finally:
        print("Done with long_worker")


async def main():
    task = asyncio.create_task(long_worker())
    await asyncio.sleep(2)
    print("Cancelling task...")
    task.cancel()

    try:
        # Wait until the "long_worker" task is cancelled
        await task
    except asyncio.CancelledError:
        print("CancelledError raised during await task")


if __name__ == "__main__":
    asyncio.run(main())
