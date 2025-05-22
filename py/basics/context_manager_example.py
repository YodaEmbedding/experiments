class ContextManager:
    def __init__(self):
        print("__init__")

    def __enter__(self):
        print("__enter__")
        return self

    def __exit__(self, *args):
        print("__exit__")

    def method(self):
        print("method")


with ContextManager() as cm:
    cm.method()
