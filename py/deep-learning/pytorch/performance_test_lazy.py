import inspect
import timeit

import torch
import torch.nn as nn

shape = 16, 192, 32, 32
ITERS_H, ITERS_W = shape[2], 4
# ITERS_H, ITERS_W = shape[2:]
time_factor = (shape[2] * shape[3]) / (ITERS_H * ITERS_W)
device = device = "cuda"
x_batch = torch.rand(shape, device=device)
conv1x1 = nn.Conv2d(192, 192, 1, padding=2, device=device).eval()
conv5x5 = nn.Conv2d(192, 192, 5, padding=2, device=device).eval()
scale_table = torch.linspace(0, 1, 64).to(device)


@torch.no_grad()
def compute(x):
    # y = x
    # y = torch.tanh(x)
    # y = torch.sigmoid(x)
    # y = torch.sigmoid(torch.tanh(x))
    # y = conv5x5(x)
    # y = conv5x5(conv5x5(x))
    # y = conv5x5(conv5x5(conv5x5(x)))
    # return y
    y = conv1x1(conv1x1(conv1x1(conv5x5(x))))
    scales = y
    indexes = scales.new_full(scales.size(), len(scale_table) - 1).int()
    # scales = scales.cpu().numpy()
    # indexes = indexes.cpu().numpy()
    # for s in scale_table[:-1].cpu().numpy():
    #     indexes -= (scales <= s).astype(int)
    for s in scale_table[:-1]:
        indexes -= (scales <= s).int()
    return indexes


def single_loop_overhead():
    for i in range(x_batch.shape[0]):
        for h in range(ITERS_H):
            for w in range(ITERS_W):
                pass


def single_sync_none():
    for i in range(x_batch.shape[0]):
        for h in range(ITERS_H):
            for w in range(ITERS_W):
                x = x_batch[i : i + 1, :, h : h + 1, w : w + 1]
                y = compute(x)


def single_sync_single():
    for i in range(x_batch.shape[0]):
        for h in range(ITERS_H):
            for w in range(ITERS_W):
                x = x_batch[i : i + 1, :, h : h + 1, w : w + 1]
                y = compute(x)
                _ = y.cpu()


def single_sync_batch():
    for h in range(ITERS_H):
        for w in range(ITERS_W):
            ys = []
            for i in range(x_batch.shape[0]):
                x = x_batch[i : i + 1, :, h : h + 1, w : w + 1]
                y = compute(x)
                ys.append(y)
            _ = [y.cpu() for y in ys]


def batch_sync_none():
    for h in range(ITERS_H):
        for w in range(ITERS_W):
            x = x_batch[:, :, h : h + 1, w : w + 1]
            y = compute(x)


def batch_sync_batch():
    for h in range(ITERS_H):
        for w in range(ITERS_W):
            x = x_batch[:, :, h : h + 1, w : w + 1]
            y = compute(x)
            _ = y.cpu()


fs = [
    "batch_sync_none",
    "batch_sync_batch",
    "single_sync_none",
    "single_sync_single",
    "single_sync_batch",
    "single_loop_overhead",
]

print(x_batch.device)
print(inspect.getsource(compute))

for name in fs:
    ts = timeit.repeat(
        f"{name}()", f"from __main__ import {name}", number=1, repeat=5
    )
    ts = [t * time_factor for t in ts]
    s = f"{name: >24}: min={min(ts):.4f} mean={sum(ts) / len(ts):.4f} max={max(ts):.4f}"
    print(s)
