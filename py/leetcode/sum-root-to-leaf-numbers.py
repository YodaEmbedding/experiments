# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def sumNumbers(self, root: Optional[TreeNode]) -> int:
        return sum(int("".join(str(x) for x in path)) for path in get_paths(root))


def get_paths(root):
    if not root.left and not root.right:
        yield (root.val,)
        return

    for node in (root.left, root.right):
        if node:
            yield from ((root.val, *path) for path in get_paths(node))


# Another alternative:
# def get_paths(root, prefix=None):
#     if prefix is None:
#         prefix = (root.val,)
#
#     if not root.left and not root.right:
#         yield prefix
#         return
#
#     for node in (root.left, root.right):
#         if node:
#             yield from get_paths(node, (*prefix, node.val))
