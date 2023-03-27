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

loss_fn = nn.CrossEntropyLoss()
model = nn.Sequential(
    nn.Linear(D_in, H), nn.ReLU(), nn.Linear(H, D_out), nn.Softmax(dim=1)
)
# model.apply(lambda m: hasattr(m, "weight") and nn.init.normal_(m.weight))

test_data = list(islice(generate_samples(seed=1234), 256))
test_inputs = torch.stack([x for x, l in test_data])
test_labels = torch.tensor([l for x, l in test_data])


def top1_accuracy() -> float:
    with torch.no_grad():
        preds = model(test_inputs)
    pred_labels = preds.detach().argmax(dim=-1)
    return (pred_labels == test_labels).sum().numpy() / len(test_labels)


optimizer = torch.optim.Adam(
    model.parameters(), lr=learning_rate, weight_decay=0.01
)
scheduler = torch.optim.lr_scheduler.ExponentialLR(optimizer, decay)

if __name__ == "__main__":
    for i, (x, label) in enumerate(generate_samples(seed=0)):
        if i == 3000:
            break

        # Forward pass
        pred = model(x.reshape(1, -1))
        pred_label = pred.detach().argmax()

        # Compute loss
        loss = loss_fn(pred, label.reshape(-1))

        # Zero accumulated gradients and perform backward pass
        optimizer.zero_grad()
        loss.backward()

        # Adjust weights
        scheduler.step()
        optimizer.step()

        if i % 10 != 0:
            continue

        accuracy = top1_accuracy()

        print(
            "{:5d} {:.1f} {:7.3f} {:.1f}".format(
                i,
                100 * accuracy,
                loss.detach().numpy(),
                math.log(scheduler.get_last_lr()[0]),
            )
        )
