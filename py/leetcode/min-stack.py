class MinStack:
    def __init__(self):
        self.stack = []
        self.stack_min = []

    def push(self, val: int) -> None:
        self.stack.append(val)
        # Store the current minimum value.
        # The minimum value is a function of the current items in the stack.
        # This "hack" works because we are only allowed to remove the
        # topmost item from the stack... Thus, the current state of the stack
        # will be exactly reachieved at some later point, and so,
        # the minimum value at push-time is the same as at pop-time.
        self.stack_min.append(min((val, *self.stack_min[-1:])))

    def pop(self) -> None:
        self.stack.pop()
        self.stack_min.pop()

    def top(self) -> int:
        return self.stack[-1]

    def getMin(self) -> int:
        return self.stack_min[-1]


# Your MinStack object will be instantiated and called as such:
# obj = MinStack()
# obj.push(val)
# obj.pop()
# param_3 = obj.top()
# param_4 = obj.getMin()
