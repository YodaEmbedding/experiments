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

        def debug_print(next_action, i, j):
            return  # Disable print.
            print(
                f"{next_action:<10}",
                f"pre[{i}]={preorder[i] if i < len(preorder) else '':<2}",
                f"in[{j}]={inorder[j] if j < len(inorder) else '':<2}",
                f"curr={stack[-1].val:<2}",
                "[" + " ".join(f"{x}" for x in traverse(stack[0])) + "]",
            )

        i = 0
        j = 0

        while True:
            # We construct the tree in the same order as preorder traversal.
            assert stack[-1].val == preorder[i]

            if preorder[i] != inorder[j]:
                # There is a node to the left of the current preorder node.
                # Thus, go left until we encounter leftmost node (inorder[j]).
                i += 1
                debug_print("leftwards", i, j)
                node = TreeNode(preorder[i])
                stack[-1].left = node
                stack.append(node)
                stack_set.add(node.val)
                continue

            # If we have reached this point, it should be impossible to go left
            # without going right first.
            # So, let's now try go right.
            # We may need to go upwards a few times first, though.

            i += 1
            j += 1

            if i == len(preorder):
                break

            while len(stack) > 1 and inorder[j] in stack_set:
                # Go up until we get to inorder[j].
                debug_print("upwards", i, j)
                node = stack.pop()
                stack_set.remove(node.val)
                if stack[-1].val == inorder[j]:
                    # If we've reached the desired in-order node,
                    # then start looking for the next in-order node.
                    j += 1

            # Must go right at least once.
            debug_print("rightwards", i, j)
            node = TreeNode(preorder[i])
            stack[-1].right = node
            stack.append(node)
            stack_set.add(node.val)

        debug_print("done", i, j)

        root, *_ = stack
        return root


# NOTE: This writeup is slightly inaccurate since the next in-order node
# need not immediately be to top-right or bottom-right...
#
# Property: There are no nodes to the left of a given node
# that has been traversed during in-order traversal.
# In fact, the immediate next in-order traversal node is
# either somewhere to the top-right or bottom-left of the current node.
#
# Example [note: bit misleading, since the next node need not be adjacent]:
# LLRLN   (current)
# LLRLRN  (bottom-right)
# LLRN    (top-right)
#
# If the next in-order node is bottom-right, the node after
# the current node in a preorder traversal must be the same
# bottom-right node.


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
