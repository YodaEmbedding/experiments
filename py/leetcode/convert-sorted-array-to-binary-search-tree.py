# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def sortedArrayToBST(self, nums: List[int]) -> Optional[TreeNode]:
        return build(nums, 0, len(nums))


def build(xs, left, right):
    length = right - left
    # A bit slower:
    # if length == 0:
    #     return None
    mid = left + length // 2
    root = TreeNode(
        val=xs[mid],
        left=build(xs, left, mid) if length > 1 else None,
        right=build(xs, mid + 1, right) if length > 2 else None,
    )
    return root
