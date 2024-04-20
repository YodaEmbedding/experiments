class LRUCache:
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.table = {}
        self.least_recent_node = None
        self.most_recent_node = None

    def get(self, key: int) -> int:
        node = self.table.get(key)
        if not node:
            return -1
        value = node.value
        self.put(key, value)
        return value

    def put(self, key: int, value: int) -> None:
        # For debugging:
        # print([x.key for x in serialize_list(self.least_recent_node, start="oldest")])
        # print([x.key for x in serialize_list(self.most_recent_node, start="newest")[::-1]])

        # Equivalent to next block, probably, but more verbose:
        # if key in self.table:
        #     node = self.table.pop(key)
        #     if node is self.least_recent_node:
        #         self.least_recent_node = node.newer
        #     if node is self.most_recent_node:
        #         self.most_recent_node = node.older
        #     node.remove()
        # if len(self.table) == self.capacity:
        #     del self.table[self.least_recent_node.key]
        #     self.least_recent_node = self.least_recent_node.newer
        #     if self.least_recent_node is None:
        #         self.most_recent_node = None

        # Remove node by key. If it isn't in the cache,
        # then remove the oldest node if capacity is exceeded.
        node = self.table.pop(key, None)
        if not node and len(self.table) == self.capacity:
            node = self.least_recent_node
            del self.table[node.key]
        if node:
            if node is self.least_recent_node:
                self.least_recent_node = node.newer
            if node is self.most_recent_node:
                self.most_recent_node = node.older
            node.remove()

        # Insert key/value into cache.
        node = LRUNode(key, value)
        if len(self.table) == 0:
            self.least_recent_node = node
        else:
            self.most_recent_node.insert_newer(node)
        self.most_recent_node = node
        self.table[key] = node


class LRUNode:
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.older = None
        self.newer = None

    def insert_newer(self, node):
        node.older = self
        node.newer = self.newer
        if self.newer:
            self.newer.older = node
        self.newer = node

    def insert_older(self, node):
        node.newer = self
        node.older = self.older
        if self.older:
            self.older.newer = node
        self.older = node

    def remove(self):
        if self.newer:
            self.newer.older = self.older
        if self.older:
            self.older.newer = self.newer
        self.newer = None
        self.older = None


def serialize_list(node, start="newest"):
    nodes = []
    while node:
        nodes.append(node)
        node = node.older if start == "newest" else node.newer
    return nodes


# Your LRUCache object will be instantiated and called as such:
# obj = LRUCache(capacity)
# param_1 = obj.get(key)
# obj.put(key,value)
