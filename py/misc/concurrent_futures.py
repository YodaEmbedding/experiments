import concurrent.futures
import time


def wait_print(t, data):
    time.sleep(t)
    print(f"t={t:.1f} {data}")
    time.sleep(t / 2)
    return f"t={1.5 * t:.1f} {data} ended"


def main():
    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
        futures = [
            executor.submit(wait_print, 1.4, "B"),
            executor.submit(wait_print, 1.0, "A"),
            executor.submit(wait_print, 2.0, "C"),
        ]

        for future in concurrent.futures.as_completed(futures):
            print(future.result())


if __name__ == "__main__":
    main()
