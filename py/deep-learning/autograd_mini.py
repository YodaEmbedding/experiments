from __future__ import annotations

from typing import Tuple, Type

import numpy as np


class Tensor:
    data: np.ndarray
    grad: Tensor | None
    creator: Type[Function] | None
    parents: Tuple[Tensor, ...]
    ctx: Context | None

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

    @property
    def shape(self):
        return self.data.shape

    def backward(self):
        for tensor in self._backward_tensors(self):
            tensor._backward_visit()

    def _backward_visit(self):
        if self.creator is None:
            return

        if self.grad is None:
            self.grad = Tensor(1)

        assert self.ctx is not None
        grad_tensors = self.creator.backward(self.ctx, self.grad)

        for parent, grad_tensor in zip(self.parents, grad_tensors):
            if grad_tensor is None:
                continue
            if parent.grad is None:
                parent.grad = Tensor(grad_tensor.data.copy())
            else:
                parent.grad.data += grad_tensor.data

    @staticmethod
    def _backward_tensors(tensor: Tensor):
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
        parents = (self, *args)
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

    def relu(self):
        return self._run_forward_op(ReLU)


class Context:
    def __init__(self, saved_tensors=()):
        self.saved_tensors = saved_tensors

    def save_for_backward(self, *args):
        self.saved_tensors = args


class Function:
    @staticmethod
    def forward(ctx: Context, *args, **kwargs) -> Tensor:
        raise NotImplementedError

    @staticmethod
    def backward(ctx: Context, *args, **kwargs) -> Tuple[Tensor | None, ...]:
        raise NotImplementedError


class Neg(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        return Tensor(-x.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        return (-grad_output,)


class Add(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        return Tensor(x.data + y.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        return grad_output, grad_output


class Sub(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        return Tensor(x.data - y.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        return grad_output, -grad_output


class Mul(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        ctx.save_for_backward(x, y)
        return Tensor(x.data * y.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        x, y = ctx.saved_tensors
        return grad_output * y, grad_output * x


class PowConst(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, const: Tensor) -> Tensor:
        ctx.save_for_backward(x, const)
        return Tensor(x.data**const.data)

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        x, const = ctx.saved_tensors
        return grad_output * const * x ** (const - 1), None


class Dot(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor, y: Tensor) -> Tensor:
        ctx.save_for_backward(x, y)
        return Tensor(x.data.dot(y.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        x, y = ctx.saved_tensors
        return grad_output * y, grad_output * x


class Sin(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.sin(x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        (x,) = ctx.saved_tensors
        return (grad_output * x.cos(),)


class Cos(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.cos(x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        (x,) = ctx.saved_tensors
        return (grad_output * -x.sin(),)


class Sum(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.sum(x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        (x,) = ctx.saved_tensors
        return (grad_output * np.ones_like(x.data),)


class ReLU(Function):
    @staticmethod
    def forward(ctx: Context, x: Tensor) -> Tensor:
        ctx.save_for_backward(x)
        return Tensor(np.maximum(0, x.data))

    @staticmethod
    def backward(ctx: Context, grad_output: Tensor):
        (x,) = ctx.saved_tensors
        return (grad_output * (x.data > 0),)


class Model:
    def __init__(self, ch_hidden=16):
        self.w1 = Tensor(np.random.randn(ch_hidden) / ch_hidden)
        self.w2 = Tensor(np.random.randn(ch_hidden) / ch_hidden)
        self.w3 = Tensor(np.random.randn(ch_hidden) / ch_hidden)

    def parameters(self):
        return [self.w1, self.w2, self.w3]

    def __call__(self, *args):
        return self.forward(*args)

    def forward(self, x: Tensor) -> Tensor:
        a = x**2
        b = (self.w1 * a + self.w3).relu()
        c = (self.w2 * a + self.w3).relu()
        d = b + c
        y_hat = d.sin() * 4
        return y_hat


def train(lr=1e-3):
    model = Model()
    losses = []

    for i in range(5000):
        x = Tensor(np.random.rand(1))
        y = (x**4).sin() * 4
        y_hat = model(x)

        mse_loss = ((y - y_hat) ** 2).sum()
        w_loss = sum(((w**2).sum() for w in model.parameters()), start=Tensor(0))
        loss = mse_loss + w_loss * 0.1
        loss.backward()
        losses.append(mse_loss.data.item())

        for param in model.parameters():
            assert param.grad is not None
            assert param.grad.shape == param.shape
            param.data -= param.grad.data * lr
            param.grad = None

        print(i, loss.data.item(), mse_loss.data.item(), w_loss.data.item())

    import matplotlib.pyplot as plt

    plt.plot(losses)
    plt.show()


def main():
    train()


if __name__ == "__main__":
    main()
