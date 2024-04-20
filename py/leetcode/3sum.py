from bisect import bisect_left
from collections import Counter


class Solution(object):
    def threeSum(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """
        results = []
        counts = Counter(nums)
        unique = sorted(counts.keys())

        # Partition into negatives and positives:
        # negatives = [x for x in unique if x <= 0]
        # positives = [x for x in unique if x >= 0]

        # Equivalently:
        zero_idx_l = bisect_left(unique, 0)
        has_zero = zero_idx_l < len(unique) and unique[zero_idx_l] == 0
        zero_idx_r = zero_idx_l + has_zero
        negatives = unique[:zero_idx_r]
        positives = unique[zero_idx_l:]

        # Results are of the form (a, b, c), where a <= b <= c.
        for a in negatives:
            for c in reversed(positives):
                b = -(a + c)
                if b > c:
                    # Break if b <= c constraint is invalidated.
                    break
                if a > b:
                    # Skip values that invalidate order constraint.
                    continue
                if counts[b] >= 1 + (b == a) + (b == c):
                    # If there are enough b's in nums, yield result.
                    results.append((a, b, c))

        return results
