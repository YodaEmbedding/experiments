from typing import Generic, TypeVar
from dataclasses import dataclass

T = TypeVar("T")

class Foo(Generic[T]):
    def __init__(self, x: T):
        self.x = x

class FooInt(Foo[int]):
    def __init__(self, x: int):
        super().__init__(x)

@dataclass
class DataclassFoo(Generic[T]):
    x: T

class DataclassFooInt(DataclassFoo[int]):
    def __init__(self, x: int):
        super().__init__(x)
