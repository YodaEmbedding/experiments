import json

def asdict(xs: list) -> dict:
    return {} if len(xs) == 0 else {xs[0]: asdict(xs[1:])}

def inverted_dict_as_tuples(d: dict, stack: list):
    for k, v in d.items():
        if len(v) == 0:
            yield (k, *reversed(stack))
        else:
            yield from inverted_dict_as_tuples(v, [*stack, k])

def inverted_dict(d: dict) -> dict:
    return {x: asdict(xs) for x, *xs in inverted_dict_as_tuples(d, [])}

d = {"a": {"b": {}}, "a1": {"b1": {"c1": {}, "d1": {}}}}
print(json.dumps(d, indent=2))

d_inv = inverted_dict(d)
print(json.dumps(d_inv, indent=2))
