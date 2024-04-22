class Solution:
    def canPlaceFlowers(self, flowerbed: List[int], n: int) -> bool:
        # return sum((run - 1) // 2 for run in get_runs(flowerbed)) >= n

        # Or, with early-exit:
        counter = 0
        for run in get_runs(flowerbed):
            counter += (run - 1) // 2
            if counter >= n:
                return True
        return False


def get_runs(xs):
    counter = 1
    for x in xs:
        if x == 0:
            counter += 1
            continue
        if counter != 0:
            yield counter
        counter = 0
    counter += 1
    yield counter
