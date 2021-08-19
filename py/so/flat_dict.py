#!/usr/bin/env python3

# https://stackoverflow.com/questions/50287479/sort-nested-dictionary-across-multiple-keys#50288131

df = {
    ("A",): {"a": {"a1": 0.5, "a2": 0.2, "a3": 1.0}},
    # ("B",): {"b1": 0.8, "b2": 0.4}
}

# def flatten(d):
#    return [(v, k) for k, v in d]

# def flatten(d):
#     return [([k, k_], v_) for k, v in d.items() for k_, v_ in v.items()]


# def flatten(d):
#    def flat(k, v):
#    #return flatten(v) if isinstance(v, dict) else [(k, v)]
#    if isinstance(v, dict):
#        return ??[k, flat(v_)) for k]

#    return k, v
#    return flatten(v) if isinstance(v, dict) else [(k, v)]
#    #def _join(

#    flat(v) for k, v in

# def f(v):
#     if !isinstance(v, dict):
#     return v

#     [(*acc, k_, f(v_)) for k_, v_ in v.items]

#     k_, v_ = f(acc,
# return

# for k, v in d.items():
#     if isinstance(v, dict):
#     ...
#     else:
#     yield k, v

# def f(d):
#     if isinstance(d, dict):
#         return [[k] + f(v) for k, v in d.items()]
#     return [d]

# print([[k] + f(v) for k, v in df.items()])
# print(f(df))

# print([flatten(v) for k, v in df.items()])
# print(flatten(df[("A",)]))

# def flatten(d):
#     print(d)
#     if isinstance(d, dict):
#         return [[[k] + acc for acc in flatten(v)] for k, v in d.items()]
#     return [[d]]

# def flat(d

# def f(acc, d):
#     # def g(v):
#     # print(d)

#     # def g():
#     #     if(isinstance(v, dict)):
#     #         return f(acc + [k]]

#     for k, v in d.items():
#         if(isinstance(v, dict)):
#             yield f(acc + [k], v)
#         else:
#             yield acc + [k, v]

# def flatten(d):
#     return [f([k], v) for k, v in d.items()]

# print(flatten(df))

# if dict:
#     for k, v in d.items():
#         yield k, *f(v)
# if terminal:
#     return ?


def flatten_dict(d_in, d_out, parent_key):
    for k, v in d_in.items():
        if isinstance(v, dict):
            flatten_dict(v, d_out, parent_key + (k,))
        else:
            d_out[parent_key + (k,)] = v


df = {
    ("A",): {"a": {"a1": 0.5, "a2": 0.2, "a3": 1.0}},
    ("B",): {"b1": 0.8, "b2": 0.4},
}

d_out = {}

flatten_dict(df, d_out, tuple())

print(d_out)

for key, value in sorted(d_out.items(), key=lambda x: x[1]):
    print(key)
    print(value)
