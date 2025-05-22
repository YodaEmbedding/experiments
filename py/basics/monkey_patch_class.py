class A:
    def __call__(self):
        return "original"

    def foo(self):
        return f"{self()}.foo"


class A(A):
    def __call__(self):
        return super().__call__() + " new"


print(A()())
print(A().foo())
