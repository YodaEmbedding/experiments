from __future__ import annotations

from typing import Tuple, Type

import numpy as np


class Tensor:
    data: np.ndarray
    grad: Tensor
    creator: Type[Function]
    parents: Tuple[Tensor, ...]
    ctx: Context

    def __init__(self, data):
        self.data = np.asanyarray(data)
        self.grad = None
        self.creator = None
        self.parents = ()
        self.ctx = None

    def __repr__(self):
        assert isinstance(self.data, np.ndarray)
        data_repr = (
            repr(self.data)
            .removeprefix("array(")
            .removesuffix(")")
            .removesuffix(", dtype=float32")
        )
        grad_fn_repr = self.creator.__name__ if self.creator else None
        return f"Tensor({data_repr}, grad_fn={grad_fn_repr})"

    def backward(self):
        for tensor in self._backwards_tensors(self):
            tensor._backward_visit()

    def _backward_visit(self):
        if self.creator is None:
            return

        if self.grad is None:
            self.grad = Tensor(1)

        print(f"* {self}\n  _.grad == None\n  _.grad = {self.grad.data}\n")
        _prefix = "    "

        grad_tensors = self.creator.backward(self.ctx, self.grad)

        for parent, grad_tensor in zip(self.parents, grad_tensors):
            if grad_tensor is None:
                continue
            if parent.grad is None:
                old_grad = None
                parent.grad = Tensor(grad_tensor.data.copy())
                print(
                    f"{_prefix}* {parent}\n"
                    f"{_prefix}  _.grad == {old_grad}\n"
                    f"{_prefix}  _.grad = {parent.grad.data}\n"
                )
            else:
                old_grad = parent.grad.data.copy()
                parent.grad.data += grad_tensor.data
                print(
                    f"{_prefix}* {parent}\n"
                    f"{_prefix}  _.grad == {old_grad}\n"
                    f"{_prefix}  _.grad += {grad_tensor.data} = {parent.grad.data}\n"
                )

    @staticmethod
    def _backwards_tensors(tensor: Tensor):
        """Reversed topological sort for reverse-mode autodiff."""
        visited = set()
        tensors = []

        def dfs(tensor: Tensor):
            if tensor in visited:
                return
            visited.add(tensor)
            for parent in tensor.parents:
                dfs(parent)
            tensors.append(tensor)

        dfs(tensor)
        return reversed(tensors)

    def _run_forward_op(self, creator: Type[Function], *args) -> Tensor:
        args = [arg if isinstance(arg, Tensor) else Tensor(arg) for arg in args]
        parents = [self, *args]
        ctx = Context()
        tensor = creator.forward(ctx, *parents)
        tensor.creator = creator
        tensor.parents = parents
        tensor.ctx = ctx
        return tensor

    def __neg__(self):
        return self._run_forward_op(Neg)

    def __add__(self, other):
        return self._run_forward_op(Add, other)

    def __sub__(self, other):
        return self._run_forward_op(Sub, other)

    def __mul__(self, other):
        return self._run_forward_op(Mul, other)

    def __pow__(self, other):
        return self._run_forward_op(PowConst, other)

    def sin(self):
        return self._run_forward_op(Sin)

    def cos(self):
        return self._run_forward_op(Cos)

    def dot(self, other):
        return self._run_forward_op(Dot, other)

    def sum(self):
        return self._run_forward_op(Sum)


class Context:
    def __init__(self, saved_tensors=()):
        self.saved_tensors = saved_tensors

    def save_for_backward(self, *args):
        self.saved_tensors = args


class Function:
    @staticmethod
    def forward(ctx: Context, *args: Tensor) -> Tensor:
        raise NotImplementedError

    @staticmethod
    def backward(ctx: Context, *args: Tensor) -> Tuple[Tensor, ...]:
        raise NotImplementedError


class Neg(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        return Tensor(-x.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        return (-grad_output,)


class Add(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        return Tensor(x.data + y.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        return grad_output, grad_output


class Sub(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        return Tensor(x.data - y.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        return grad_output, -grad_output


class Mul(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        ctx.save_for_backward(x, y)
        return Tensor(x.data * y.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        x, y = ctx.saved_tensors
        return grad_output * y, grad_output * x


class PowConst(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, const: Tensor) -> Tensor:
        ctx.save_for_backward(x, const)
        return Tensor(x.data**const.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        x, const = ctx.saved_tensors
        return grad_output * const * x ** (const - 1), None


class Dot(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        ctx.save_for_backward(x, y)
        return Tensor(x.data.dot(y.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        x, y = ctx.saved_tensors
        return grad_output * y, grad_output * x


class Sin(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.sin(x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        (x,) = ctx.saved_tensors
        return (grad_output * x.cos(),)


class Cos(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.cos(x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        (x,) = ctx.saved_tensors
        return (grad_output * -x.sin(),)


class Sum(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.sum(x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        (x,) = ctx.saved_tensors
        return (grad_output * np.ones_like(x.data),)


def print_tensors(*args):
    names = "abcdefghijklmnopqrstuvwxyz"[: len(args)]

    for name, tensor in zip(names, args):
        # print(f"{name} = {tensor}")

        import torch

        if isinstance(tensor, torch.Tensor):
            tensor.retain_grad()

    print("----------------")
    args[-1].backward()

    for name, tensor in zip(names, args):
        print(f"{name}.grad = {tensor.grad}")


def func(a, b, c):
    d = a * b
    e = d + d * c - d * a**4 + (c**3 - 84).sin()
    f = e + e
    g = f + f
    h = c.dot(g)
    i = h.sum()
    return a, b, c, d, e, f, g, h, i


def main():
    a = Tensor(np.array([2, 5], dtype=np.float32))
    b = Tensor(np.array([5, 7], dtype=np.float32))
    c = Tensor(np.array([11, 9], dtype=np.float32))
    args = func(a, b, c)
    print_tensors(*args)

    print("\n================\n")

    import torch

    a = torch.tensor(a.data, dtype=torch.float32, requires_grad=True)
    b = torch.tensor(b.data, dtype=torch.float32, requires_grad=True)
    c = torch.tensor(c.data, dtype=torch.float32, requires_grad=True)
    args = func(a, b, c)
    print_tensors(*args)


if __name__ == "__main__":
    main()
