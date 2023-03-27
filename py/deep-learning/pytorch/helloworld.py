#!/usr/bin/env python

import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim

torch.manual_seed(1)

# data = torch.randn(2, 5)
# lin = nn.Linear(5, 3)
# print(data)
# print(lin(data))

x = torch.Tensor(5, 3).uniform_(0, 2)
y = torch.Tensor(5, 3).uniform_(-2, 0)
result = y.add_(x)
# result = torch.Tensor(5, 3)
# torch.add(x, y, out=result)

print(result)
print(result[::2])

x_data = [1.0, 2.0, 3.0]
y_data = [2.0, 4.0, 6.0]
