import collections

import yaml
from yaml import MappingNode
from yaml.constructor import ConstructorError


class Loader(yaml.SafeLoader):
    def construct_mapping(self, node, deep=False):
        if not isinstance(node, MappingNode):
            raise ConstructorError(
                None,
                None,
                "expected a mapping node, but found %s" % node.id,
                node.start_mark,
            )
        mapping = {}
        for key_node, value_node in node.value:
            key = self.construct_object(key_node, deep=deep)
            if not isinstance(key, collections.abc.Hashable):
                raise ConstructorError(
                    "while constructing a mapping",
                    node.start_mark,
                    "found unhashable key",
                    key_node.start_mark,
                )
            # CUSTOM VERSION KEY HANDLING:
            if key == "version":
                value = value_node.value
            else:
                value = self.construct_object(value_node, deep=deep)
            mapping[key] = value
        return mapping


with open("example.yaml") as f:
    d = yaml.load(f, Loader=Loader)

print(d)
assert d == {"version": "0.20", "some_number": 0.2}
