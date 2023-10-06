import gc


def print_objs(obj_ids):
    for obj in gc.get_objects():
        if id(obj) in obj_ids or obj is obj_ids:
            continue
        print(id(obj), obj)


obj_ids = set(id(obj) for obj in gc.get_objects())
[(print_objs(obj_ids), i)[1] for i in range(8)]
