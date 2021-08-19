#!/usr/bin/env python3

import socket
import itertools

IP = "0.0.0.0"
PORT = 5678
BUFFER_SIZE = 1024


def main():
    num_bytes_recv = 0
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((IP, PORT))
    sock.listen(1)
    conn, addr = sock.accept()
    print(conn, addr)

    for i in itertools.count():
        data = conn.recv(BUFFER_SIZE)
        num_bytes_recv += len(data)
        if data == b"":
            break
        print(i, num_bytes_recv, repr(data.decode("utf-8")[:64]))

    sock.close()


if __name__ == "__main__":
    main()
