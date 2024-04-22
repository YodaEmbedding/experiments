from math import ceil


class Solution:
    def largestNumber(self, nums: List[int]) -> str:
        nums = [str(x) for x in nums]
        max_len = max(len(x) for x in nums)
        cycle_twice = lambda x: (x * ceil(2 * max_len / len(x)))[: 2 * max_len]
        nums.sort(key=cycle_twice, reverse=True)
        if nums[0] == nums[-1] == "0":
            return "0"
        return "".join(nums)
