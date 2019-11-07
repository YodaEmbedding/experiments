#!/usr/bin/env python3

import itertools
import socket
from typing import ByteString

IP = '0.0.0.0'
PORT = 5678

def read_eol(conn):
    return conn.recv(1, socket.MSG_WAITALL) == b'\x00'

def read_fixed_message(conn) -> ByteString:
    msg_len_buf = conn.recv(4, socket.MSG_WAITALL)
    if len(msg_len_buf) != 4 or not read_eol(conn):
        return None
    msg_len = int.from_bytes(msg_len_buf, byteorder='big')
    buf = conn.recv(msg_len, socket.MSG_WAITALL)
    if len(buf) < msg_len or not read_eol(conn):
        return None
    return buf

def str_preview(s, max_len=16):
    if len(s) < max_len:
        return s
    return f'{s[:max_len - 6]}...{s[-3:]}'

def main():
    while True:
        print('Waiting for connection...')
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.bind((IP, PORT))
        sock.listen(1)
        conn, addr = sock.accept()
        print(f'Established connection on\n{conn}\n{addr}')

        for i in itertools.count():
            # msg = read_fixed_message(conn)
            msg = conn.recv(1, socket.MSG_WAITALL)
            if len(msg) == 0:
                break

            data = msg
            print(i, len(data), str_preview(data))

        sock.close()
        print('Connection closed')

if __name__ == '__main__':
    main()
