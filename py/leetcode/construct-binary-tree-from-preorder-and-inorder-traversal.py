# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def buildTree(self, preorder: List[int], inorder: List[int]) -> Optional[TreeNode]:
        stack = [TreeNode(preorder[0])]
        stack_set = {stack[-1].val}

        i = 0
        j = 0

        def debug_print(tag, i, j):
            print(
                f"{tag:<10}",
                i,
                j,
                preorder[i] if i < len(preorder) else None,
                inorder[j] if j < len(inorder) else None,
                [x.val for x in stack],
                "[" + " ".join(f"{x}" for x in traverse(stack[0])) + "]",
            )

        while True:
            assert stack[-1].val == preorder[i]

            # Go left until we hit the bottom-left.
            while stack[-1].val != inorder[j]:
                i += 1
                # Go left.
                debug_print("leftwards", i, j)
                node = TreeNode(preorder[i])
                stack[-1].left = node
                stack.append(node)
                stack_set.add(node.val)

            assert stack[-1].val == inorder[j]

            i += 1

            # Go up until we find a node with a non-empty right.
            # while j + 1 < len(inorder) and len(stack) > 1:
            while True:
                # if j + 2 >= len(inorder) or (len(stack) > 1 and stack[-2].val == inorder[j + 1]):
                if len(stack) == 1 or inorder[j + 1] not in stack_set:
                    j += 1
                    # Go right.
                    debug_print("rightwards", i, j)
                    node = TreeNode(preorder[i])
                    stack[-1].right = node
                    stack.append(node)
                    stack_set.add(node.val)
                    break
                if len(stack) > 1 and stack[-2].val == inorder[j + 1]:
                    # Go up.
                    debug_print("upwards", i, j)
                    node = stack.pop()
                    stack_set.remove(node.val)
                    j += 1
                    continue

            # assert stack[-1].val == inorder[j]

            # if i == len(preorder):
            #     break

            debug_print("stay", i, j)

            # Exit if no more nodes.
            if j == len(inorder) or i == len(preorder):
                # assert i == len(preorder)
                # assert j == len(inorder)
                break

            print("---")

        root, *_ = stack
        return root


def traverse(node):
    yield node.val
    if node.left is not None:
        yield "L=["
        yield from traverse(node.left)
        yield "]"
    if node.right is not None:
        yield "R=["
        yield from traverse(node.right)
        yield "]"
