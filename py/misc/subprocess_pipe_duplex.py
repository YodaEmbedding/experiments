import os
import subprocess
from queue import Queue
from threading import Thread
from time import sleep

# args = ["cat"]
# args = ["awk", "{print;print}"]
args = ["awk", '{ print "LINE BRK"; print; system("") }']
# args = ["rev"]
process = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE)

# queue = Queue()


def encoder(writer):
    print("encoder started")
    xs = [b"this\nis\nsparta\n", b"once\nupon\n", b"a\nmidnight\ndreary\n"]
    # while True:
    for x in xs:
        print(f"writing {x}")
        writer.write(x)
        writer.flush()
        sleep(1)

    # writer.close()


def decoder(reader):
    print("decoder started")

    while True:
        x = reader.read1(8)
        print(f"read {x}")

    # for line in reader:
    #     print(f"read {line}")


threads = [
    Thread(target=decoder, name="decoder", args=(process.stdout,)),
    Thread(target=encoder, name="encoder", args=(process.stdin,)),
]

# import time
for thread in threads:
    thread.start()
    # time.sleep(1)

# process.stdin.write(b"a")
# process.stdin.flush()
# # process.stdin.write(b"hello\nthere\n")
# print(process.stdout.read(1))
# process.stdin.write(b"bc")
# process.stdin.flush()
# print(process.stdout.read(1))
# process.stdin.write(b"def")
# process.stdin.flush()
# print(process.stdout.read(1))
# print(process.stdout.read(1))
# print(process.stdout.read(1))
# print(process.stdout.read(1))

# process.stdin.write(b"hello\nthere\n")
# process.stdin.flush()
# print(process.stdout.readline())
# print(process.stdout.readline())

# process.stdin.write(b"well\n")
# process.stdin.flush()
# print(process.stdout.read1(1024))
#
# process.stdin.write(b"hello\nthere\n")
# process.stdin.flush()
# print(process.stdout.read1(1024))
#
# process.stdin.write(b"A star shines on the hour of our meeting\n")
# process.stdin.write(b"Elen sila lumenn omentielvo\n")
# process.stdin.flush()
# print(process.stdout.read1(1024))

# process.stdin.write(b"h")
# x = process.stdout.read()
# print(x)
