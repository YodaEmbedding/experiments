class MyLinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def get(self, index: int) -> int:
        # if not self.head:
        #     return -1
        node = self.head and self.head.get_next(index)
        return node.val if node else -1

    def addAtHead(self, val: int) -> None:
        self.head = Node(val, next=self.head)
        if not self.tail:
            self.tail = self.head

    def addAtTail(self, val: int) -> None:
        node = Node(val)
        if not self.tail:
            self.head = node
            self.tail = node
            return
        self.tail.next = node
        self.tail = node

    def addAtIndex(self, index: int, val: int) -> None:
        if index == 0:
            self.addAtHead(val)
            return
        if not self.head:
            return
        prev = self.head.get_next(index - 1)
        if not prev:
            return
        if prev is self.tail:
            self.addAtTail(val)
            return
        prev.next = Node(val, next=prev.next)
        # Another valid alternative:
        # if prev is self.tail:
        #     self.tail = prev.next

    def deleteAtIndex(self, index: int) -> None:
        if not self.head:
            return
        if index == 0:
            if self.head is self.tail:
                self.head = None
                self.tail = None
                return
            self.head = self.head.next
        prev = self.head.get_next(index - 1)
        if not prev:
            return
        if not prev.next:
            return
        if prev.next is self.tail:
            self.tail = prev
        prev.next = prev.next.next


class Node:
    def __init__(self, val, next=None):
        self.val = val
        self.next = next

    def get_next(self, index: int) -> "Node":
        node = self
        for _ in range(index):
            node = node.next
            if not node:
                break
        return node


# Your MyLinkedList object will be instantiated and called as such:
# obj = MyLinkedList()
# param_1 = obj.get(index)
# obj.addAtHead(val)
# obj.addAtTail(val)
# obj.addAtIndex(index,val)
# obj.deleteAtIndex(index)
