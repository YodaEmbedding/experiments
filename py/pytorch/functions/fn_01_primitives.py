import math
from itertools import islice

import torch
from torch import nn

from utils import generate_samples

torch.manual_seed(0)

D_in = 10
H = 100  # TODO question: why do H=3 and H=4 produce bad results?
D_out = 3
decay = 0.999
learning_rate = 1e-1

w1 = torch.normal(0, 1, (D_in, H), requires_grad=True)
w2 = torch.normal(0, 1, (H, D_out), requires_grad=True)
loss_fn = nn.CrossEntropyLoss()


def forward(x: torch.Tensor) -> torch.Tensor:
    x = x.mm(w1).clamp(min=0).mm(w2).softmax(dim=-1)
    return x


test_data = list(islice(generate_samples(seed=1234), 256))
test_inputs = torch.stack([x for x, l in test_data])
test_labels = torch.tensor([l for x, l in test_data])


def top1_accuracy() -> float:
    with torch.no_grad():
        preds = forward(test_inputs)
    pred_labels = preds.detach().argmax(dim=-1)
    return (pred_labels == test_labels).sum().numpy() / len(test_labels)


if __name__ == "__main__":
    for i, (x, label) in enumerate(generate_samples(seed=0)):
        if i == 3000:
            break

        # Forward pass
        pred = forward(x.reshape(1, -1))
        pred_label = pred.detach().argmax()

        # Compute loss
        weight_sum = (w1 ** 2).sum() + (w2 ** 2).sum()
        weight_loss = 0.01 * weight_sum
        accuracy_loss = loss_fn(pred, label.reshape(-1))
        loss = accuracy_loss + weight_loss
        loss.backward()

        # Adjust weights
        with torch.no_grad():
            w1 -= learning_rate * w1.grad
            w2 -= learning_rate * w2.grad
            w1.grad.zero_()
            w2.grad.zero_()

        learning_rate *= decay

        if i % 10 != 0:
            continue

        accuracy = top1_accuracy()

        print(
            "{:5d} {:.1f} {:7.3f} {:7.3f} {:.1f}".format(
                i,
                100 * accuracy,
                loss.detach().numpy(),
                accuracy_loss.detach().numpy(),
                math.log(learning_rate),
            )
        )
