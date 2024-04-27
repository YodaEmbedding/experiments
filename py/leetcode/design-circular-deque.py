class MyCircularDeque:
    def __init__(self, k: int):
        self.xs = [None] * k
        self.front_idx = 0
        self.last_idx = 0
        self.num_items = 0

    def insertFront(self, value: int) -> bool:
        if self.isFull():
            return False
        self.xs[self.front_idx - 1] = value
        self.front_idx = (self.front_idx - 1) % len(self.xs)
        self.num_items += 1
        return True

    def insertLast(self, value: int) -> bool:
        if self.isFull():
            return False
        self.xs[self.last_idx] = value
        self.last_idx = (self.last_idx + 1) % len(self.xs)
        self.num_items += 1
        return True

    def deleteFront(self) -> bool:
        if self.isEmpty():
            return False
        self.front_idx = (self.front_idx + 1) % len(self.xs)
        self.num_items -= 1
        return True

    def deleteLast(self) -> bool:
        if self.isEmpty():
            return False
        self.last_idx = (self.last_idx - 1) % len(self.xs)
        self.num_items -= 1
        return True

    def getFront(self) -> int:
        return -1 if self.isEmpty() else self.xs[self.front_idx]

    def getRear(self) -> int:
        return -1 if self.isEmpty() else self.xs[self.last_idx - 1]

    def isEmpty(self) -> bool:
        return self.num_items == 0

    def isFull(self) -> bool:
        return self.num_items == len(self.xs)


# Your MyCircularDeque object will be instantiated and called as such:
# obj = MyCircularDeque(k)
# param_1 = obj.insertFront(value)
# param_2 = obj.insertLast(value)
# param_3 = obj.deleteFront()
# param_4 = obj.deleteLast()
# param_5 = obj.getFront()
# param_6 = obj.getRear()
# param_7 = obj.isEmpty()
# param_8 = obj.isFull()
