from __future__ import annotations

from typing import Tuple, Type

import numpy as np

# TODO this GradTensor thing is a hack... Maybe a mode...
# TODO grad_fn?


class Tensor:
    data: np.ndarray
    grad: Tensor
    parents: Tuple[Tensor, ...]
    creator: Type[Function]
    ctx: Context

    def __init__(self, data):
        self.data = np.asanyarray(data)
        self.grad = None
        self.creator = None
        self.parents = None
        self.ctx = None

    def __repr__(self):
        assert isinstance(self.data, np.ndarray)
        return f"Tensor({self.data}, dtype={self.data.dtype})"

    def backward(self):
        if self.creator is None:
            return

        if self.grad is None:
            self.grad = Tensor(1)

        grad_tensors = self.creator.backward(self.ctx, GradTensor(self.grad))

        for parent, grad_tensor in zip(self.parents, grad_tensors):
            if grad_tensor is None:
                continue
            parent.grad = grad_tensor.tensor
            parent.backward()

    def _run_forward_op(self, creator: Type[Function], *args: Tensor) -> Tensor:
        print(f"Running {creator.__name__} on {args}")
        args = (self, *args)
        compute_args = [GradTensor(tensor) for tensor in args]
        ctx = Context()
        tensor = creator.forward(ctx, *compute_args)
        assert isinstance(tensor, GradTensor)
        tensor = tensor.tensor
        tensor.creator = creator
        tensor.parents = args
        tensor.ctx = ctx
        return tensor

    def __add__(self, other: Tensor) -> Tensor:
        return self._run_forward_op(Add, other)

    def __sub__(self, other: Tensor) -> Tensor:
        return self._run_forward_op(Sub, other)

    def __mul__(self, other: Tensor) -> Tensor:
        return self._run_forward_op(Mul, other)

    def __pow__(self, const: int) -> Tensor:
        return self._run_forward_op(PowConst, Tensor(const))

    def sin(self) -> Tensor:
        return self._run_forward_op(Sin)

    def cos(self) -> Tensor:
        raise NotImplementedError

    def dot(self, other: Tensor) -> Tensor:
        return self._run_forward_op(Dot, other)


class GradTensor(Tensor):
    def __init__(self, tensor: Tensor):
        self.tensor = tensor
        # super().__init__(tensor.data)

    def __repr__(self):
        return f"GradTensor({self.tensor})"

    @property
    def data(self):
        return self.tensor.data

    @property
    def grad(self):
        return self.tensor.grad

    def __add__(self, other: GradTensor):
        if not isinstance(other, Tensor):
            other = GradTensor(Tensor(other))
        return GradTensor(Tensor(self.data + other.data))

    def __sub__(self, other: GradTensor):
        if not isinstance(other, Tensor):
            other = GradTensor(Tensor(other))
        return GradTensor(Tensor(self.data - other.data))

    def __mul__(self, other: GradTensor):
        if not isinstance(other, Tensor):
            other = GradTensor(Tensor(other))
        return GradTensor(Tensor(self.data * other.data))

    def __pow__(self, other: GradTensor):
        if not isinstance(other, Tensor):
            other = GradTensor(Tensor(other))
        return GradTensor(Tensor(self.data**other.data))

    def sin(self):
        return GradTensor(Tensor(np.sin(self.data)))

    def cos(self):
        return GradTensor(Tensor(np.cos(self.data)))

    def dot(self, other: GradTensor):
        return GradTensor(Tensor(self.data.dot(other.data)))


class Context:
    def __init__(self):
        self.saved_tensors = None

    def save_for_backward(self, *args):
        self.saved_tensors = args


class Function:
    @staticmethod
    def forward(ctx: Context, *args: Tensor) -> Tensor:
        raise NotImplementedError

    @staticmethod
    def backward(ctx: Context, *args: Tensor) -> Tuple[Tensor, ...]:
        raise NotImplementedError


class Add(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        return x + y

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        return grad_output, grad_output


class Sub(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        return x - y

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        return grad_output, -grad_output


class Mul(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        ctx.save_for_backward(x, y)
        return x * y

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        x, y = ctx.saved_tensors
        return grad_output * y, grad_output * x


class PowConst(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, const: int) -> Tensor:
        ctx.save_for_backward(x, const)
        return x**const

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        x, const = ctx.saved_tensors
        return grad_output * const * x ** (const - 1), None


class Dot(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        ctx.save_for_backward(x, y)
        return x.dot(y)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        x, y = ctx.saved_tensors
        return grad_output * y, grad_output * x


class Sin(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return x.sin()

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor) -> Tuple[Tensor, ...]:
        (x,) = ctx.saved_tensors
        return (grad_output * x.cos(),)


def print_tensors(a, b, c, d, e):
    names = "abcde"

    print(", ".join(f"{name}" for name in names))
    for tensor in (a, b, c, d, e):
        print(tensor)

    print("e.backward()")
    e.backward()

    print(", ".join(f"{name}.grad" for name in names))
    for tensor in (a, b, c, d, e):
        print(tensor.grad)


def main():
    z = Tensor(np.array([1, 2, 3, 4]))
    a = Tensor(np.array([0, 1, 4, 1]))
    b = Tensor(np.array([0, 3, 4, 9]))
    c = a + b
    d = c**2
    # e = d + d + a
    e = d.dot(z)
    # e = e.sin()

    print_tensors(a, b, c, d, e)
    print("================")

    import torch

    z = torch.tensor([1, 2, 3, 4], dtype=torch.float32, requires_grad=True)
    a = torch.tensor([0, 1, 4, 1], dtype=torch.float32, requires_grad=True)
    b = torch.tensor([0, 3, 4, 9], dtype=torch.float32, requires_grad=True)
    c = a + b
    d = c**2
    e = c.dot(z)
    # e = e.sin()

    c.retain_grad()
    d.retain_grad()
    e.retain_grad()

    print_tensors(a, b, c, d, e)


if __name__ == "__main__":
    main()


# Track inputs and outputs of functions
#
