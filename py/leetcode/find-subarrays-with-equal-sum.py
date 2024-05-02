class Solution:
    def findSubarrays(self, nums: List[int]) -> bool:
        seen = set()
        nums_drop1 = iter(nums)
        next(nums_drop1)

        for a, b in zip(nums, nums_drop1):
            value = a + b
            if value in seen:
                return True
            seen.add(value)

        return False
