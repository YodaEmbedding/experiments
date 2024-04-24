class Solution:
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        num_table = {x: i for i, x in enumerate(nums)}
        i = next(i for i, x in enumerate(nums) if i != num_table.get(target - x, i))
        j = num_table[target - nums[i]]
        return i, j
