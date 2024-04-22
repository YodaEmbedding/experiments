# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def isBalanced(self, root: Optional[TreeNode]) -> bool:
        is_balanced, _ = check(root)
        return is_balanced


def check(root):
    if not root:
        return True, 0
    is_balanced_left, depth_left = check(root.left)
    if not is_balanced_left:
        return False, None
    is_balanced_right, depth_right = check(root.right)
    if not is_balanced_right:
        return False, None
    is_balanced = abs(depth_left - depth_right) <= 1
    depth = 1 + max(depth_left, depth_right)
    return is_balanced, depth


# SLOW SOLUTION (recomputes depth):
#
# class Solution:
#     def isBalanced(self, root: Optional[TreeNode]) -> bool:
#         return not root or (
#             abs(get_depth(root.left) - get_depth(root.right)) <= 1
#             and self.isBalanced(root.left)
#             and self.isBalanced(root.right)
#         )
#
#
# def get_depth(root):
#     if not root:
#         return 0
#     return 1 + max(get_depth(root.left), get_depth(root.right))
