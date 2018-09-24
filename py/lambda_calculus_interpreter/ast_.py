from dataclasses import dataclass

@dataclass
class AppNode:
    lhs: object
    rhs: object

    def __str__(self):
        return f'{self.lhs} {self.rhs}'

@dataclass
class AbsNode:
    name: object
    body: object

    def __str__(self):
        return f'(λ{self.name}.{self.body})'

@dataclass
class IdNode:
    name: str

    def __str__(self):
        return self.name
