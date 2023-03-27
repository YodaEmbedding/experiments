import math
from itertools import islice

import torch
from torch import nn

torch.manual_seed(0)


def top1_accuracy() -> float:
    with torch.no_grad():
        preds = model(test_inputs)
    pred_labels = preds.detach().argmax(dim=-1)
    return (pred_labels == test_labels).sum().numpy() / len(test_labels)


if __name__ == "__main__":
    pass

    # for i, (x, label) in enumerate(generate_samples(seed=0)):
    #     if i == 3000:
    #         break
    #
    #     # Forward pass
    #     pred = model(x.reshape(1, -1))
    #     pred_label = pred.detach().argmax()
    #
    #     # Compute loss
    #     weight_sum = (w1 ** 2).sum() + (w2 ** 2).sum()
    #     weight_loss = 0.01 * weight_sum
    #     accuracy_loss = loss_fn(pred, label.reshape(-1))
    #     loss = accuracy_loss + weight_loss
    #     optimizer.zero_grad()
    #     loss.backward()
    #
    #     # Adjust weights
    #     scheduler.step()
    #     optimizer.step()
    #
    #     if i % 10 != 0:
    #         continue
    #
    #     accuracy = top1_accuracy()
    #
    #     print(
    #         "{:5d} {:.1f} {:7.3f} {:7.3f} {:.1f}".format(
    #             i,
    #             100 * accuracy,
    #             loss.detach().numpy(),
    #             accuracy_loss.detach().numpy(),
    #             math.log(scheduler.get_last_lr()[0]),
    #         )
    #     )
