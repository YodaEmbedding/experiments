class Solution:
    def numBusesToDestination(
        self, routes: List[List[int]], source: int, target: int
    ) -> int:
        if source == target:
            return 0

        route_lookup = {i: set(route) for i, route in enumerate(routes)}
        graph = build_graph(route_lookup)
        source_route_ids = []
        target_route_ids = []

        for i, route in route_lookup.items():
            if source in route:
                source_route_ids.append(i)
            if target in route:
                target_route_ids.append(i)

        visited = set()
        frontier = deque()

        visited.update(source_route_ids)
        frontier.extend([(node, 1) for node in source_route_ids])

        while frontier:
            node, num_hops = frontier.popleft()
            if node in target_route_ids:
                return num_hops
            for next_node in graph[node]:
                if next_node in visited:
                    continue
                visited.add(next_node)
                frontier.append((next_node, num_hops + 1))

        return -1


def build_graph(route_lookup):
    graph = defaultdict(list)
    for i, route_i in route_lookup.items():
        for j, route_j in route_lookup.items():
            if i >= j or route_i.isdisjoint(route_j):
                continue
            graph[i].append(j)
            graph[j].append(i)
    return graph


# Attempt to speed up set intersection / graph building:
#
# def build_graph(route_lookup):
#     graph = defaultdict(set)
#     stop_lookup = defaultdict(set)
#
#     for route_id, route in route_lookup.items():
#         for stop in route:
#             stop_lookup[stop].add(route_id)
#
#     for route_ids in stop_lookup.values():
#         for route_id_i in route_ids:
#             for route_id_j in route_ids:
#                 graph[route_id_i].add(route_id_j)
#
#     return graph
