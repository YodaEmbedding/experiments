class Solution:
    def permute(self, nums: List[int]) -> List[List[int]]:
        # Easy solution:
        # return list(itertools.permutations(nums))

        # Manual solution:
        return list(self.iter_permute(nums))

    def iter_permute(self, xs):
        graph = {i: (i - 1, i + 1) for i in range(-1, len(xs) + 1)}
        indexes = []
        values = []
        end_index = False

        while True:
            if len(indexes) == len(xs):
                yield tuple(values)
                end_index = True

            if end_index:
                end_index = False

                if not indexes:
                    return

                # Place current index back into pool.
                curr = indexes[-1]
                curr_prev, curr_next = graph[curr]
                prev_prev, _ = graph[curr_prev]
                _, next_next = graph[curr_next]
                graph[curr_prev] = (prev_prev, curr)
                graph[curr_next] = (curr, next_next)

                # If pool is exhausted, "end" the loop for the current index.
                if curr_next == len(xs):
                    indexes.pop()
                    values.pop()
                    end_index = True
                    continue

                # Withdraw next available index from pool.
                curr = curr_next
                indexes[-1] = curr
                values[-1] = xs[curr]

                curr_prev, curr_next = graph[curr]
                prev_prev, _ = graph[curr_prev]
                _, next_next = graph[curr_next]
                graph[curr_prev] = (prev_prev, curr_next)
                graph[curr_next] = (curr_prev, next_next)

                continue

            if len(indexes) < len(xs):
                # Withdraw next available index from pool.
                _, curr = graph[-1]
                indexes.append(curr)
                values.append(xs[curr])

                curr_prev, curr_next = graph[curr]
                prev_prev, _ = graph[curr_prev]
                _, next_next = graph[curr_next]
                graph[curr_prev] = (prev_prev, curr_next)
                graph[curr_next] = (curr_prev, next_next)

                continue
