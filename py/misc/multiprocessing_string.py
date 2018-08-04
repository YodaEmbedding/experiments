from multiprocessing import Array, Process
from ctypes import c_wchar
from time import sleep

def run(shared_str):
    while True:
        print(shared_str[:])
        sleep(0.05)

def strcpy(shared_str, s):
    shared_str[:len(s)] = s
    shared_str[len(s)] = '\0'

shared_str = Array(c_wchar, "\0" * 1024)
p = Process(target=run, args=(shared_str,))
p.start()

for i in range(50, -1, -1):
    strcpy(shared_str, str(i))
    sleep(0.05)
