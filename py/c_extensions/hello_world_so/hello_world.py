import ctypes
import os

so_file = "hello_world.so"
so_dir = os.path.dirname(__file__)
so_path = os.path.abspath(os.path.join(so_dir, so_file))
hello_world = ctypes.CDLL(so_path)

hello_world.say_hello()
