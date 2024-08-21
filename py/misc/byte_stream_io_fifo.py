from collections import deque
from io import BytesIO
from timeit import timeit


class FifoBufferedReaderWriter:
    """
    https://stackoverflow.com/a/78895090/365102
    """

    def __init__(self, chunk_size=1024):
        self.available = 0
        self.chunk_size = chunk_size
        self._queue = deque()
        self._write_buffer = BytesIO()

    def __len__(self):
        return self.available

    def write(self, data: bytes, *, flush=False):
        offset = 0
        buf_size = self._write_buffer.tell()

        if buf_size > 0:
            # Fill up the buffer.
            chunk = data[: self.chunk_size - buf_size]
            offset += len(chunk)
            self._write_buffer.write(chunk)

        if self._write_buffer.tell() == self.chunk_size:
            # Flush the buffer if it is full.
            self._flush_write_buffer()

        stop_offset = offset + (len(data) - offset) // self.chunk_size * self.chunk_size

        while offset < stop_offset:
            # Write data in chunks of chunk_size.
            chunk = data[offset : offset + self.chunk_size]
            offset += len(chunk)
            self._queue_append(chunk)

        # Write any remaining data.
        chunk = data[offset:]

        if len(chunk) == 0:
            return

        if flush:
            self._queue_append(chunk)
        else:
            self._write_buffer.write(chunk)

    def flush(self):
        self._flush_write_buffer()

    def read(self, size: int):
        with BytesIO() as buffer:
            self._read_into_buffer(size, buffer)
            return buffer.getvalue()

    def _queue_append(self, chunk):
        self._queue.append(chunk)
        self.available += len(chunk)

    def _queue_appendleft(self, chunk):
        self._queue.appendleft(chunk)
        self.available += len(chunk)

    def _queue_popleft(self):
        chunk = self._queue.popleft()
        self.available -= len(chunk)
        return chunk

    def _flush_write_buffer(self):
        chunk = self._write_buffer.getvalue()
        self._write_buffer.seek(0)
        self._write_buffer.truncate()
        self._queue_append(chunk)

    def _read_into_buffer(self, size, buffer):
        buffer_size = 0

        # Assemble chunks into output buffer.
        while buffer_size < size:
            try:
                chunk = self._queue_popleft()
            except IndexError:  # Or queue.Empty, if using queue.Queue.
                raise RuntimeError("Not enough data to read.")
            needed = size - buffer_size

            if len(chunk) > needed:
                self._queue_appendleft(chunk[needed:])
                chunk = chunk[:needed]

            buffer.write(chunk)
            buffer_size += len(chunk)


class FifoFileBuffer(object):
    """
    https://stackoverflow.com/a/10917767/365102
    """

    def __init__(self):
        self.buf = BytesIO()
        self.available = 0  # Bytes available for reading
        self.size = 0
        self.write_fp = 0

    def read(self, size=None):
        """Reads size bytes from buffer"""
        if size is None or size > self.available:
            size = self.available
        size = max(size, 0)

        result = self.buf.read(size)
        self.available -= size

        if len(result) < size:
            self.buf.seek(0)
            result += self.buf.read(size - len(result))

        return result

    def write(self, data):
        """Appends data to buffer"""
        if self.size < self.available + len(data):
            # Expand buffer
            new_buf = BytesIO()
            new_buf.write(self.read())
            self.write_fp = self.available = new_buf.tell()
            read_fp = 0
            while self.size <= self.available + len(data):
                self.size = max(self.size, 1024) * 2
            new_buf.write(b"0" * (self.size - self.write_fp))
            self.buf = new_buf
        else:
            read_fp = self.buf.tell()

        self.buf.seek(self.write_fp)
        written = self.size - self.write_fp
        self.buf.write(data[:written])
        self.write_fp += len(data)
        self.available += len(data)
        if written < len(data):
            self.write_fp -= self.size
            self.buf.seek(0)
            self.buf.write(data[written:])
        self.buf.seek(read_fp)

    def flush(self):
        pass


def test(stream):
    k1 = 100
    k2 = 100
    write_size = 1024
    read_size = 512
    for _ in range(k1):
        for _ in range(k2):
            stream.write(b"-" * write_size)
        stream.flush()
        for _ in range(k2):
            stream.read(write_size - read_size)
    stream.read(stream.available)


def main():
    n = 10

    funcs = [
        FifoBufferedReaderWriter,
        FifoFileBuffer,
    ]

    for func in funcs:
        stream = func()
        t = timeit("test(stream)", number=n, globals={"test": test, "stream": stream})
        print(f"{func.__name__:<40} t={t / n:.6f}")


if __name__ == "__main__":
    main()
