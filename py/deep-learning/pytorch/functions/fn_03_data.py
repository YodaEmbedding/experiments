import math
from functools import partial
from itertools import chain, islice, repeat

import numpy as np
import torch
from torch import nn
from torch.utils.data import DataLoader, Dataset
from utils import generate_samples

# TODO
# use_cuda = torch.cuda.is_available()
# device = torch.device("cuda:0" if use_cuda else "cpu")
# torch.backends.cudnn.benchmark = True

torch.manual_seed(0)

D_in = 10
H = 100  # TODO question: why do H=3 and H=4 produce bad results?
D_out = 3
decay = 0.999
learning_rate = 1e-1


class PolynomialDataset(Dataset):
    def __init__(self, points, transform=None):
        t = np.linspace(0, 1, points, dtype=np.float32)
        self.t = t
        self.samples = [
            0.5 * np.ones_like(t),
            t,
            t**2,  #
        ]
        self.labels = [0, 1, 2]
        identity = lambda x: x
        self.transform = identity if transform is None else transform

    def __getitem__(self, index):
        sample = self.transform(self.samples[index])
        label = self.labels[index]
        return sample, label

    def __len__(self):
        return len(self.samples)


class AdditiveGaussianNoise:
    def __init__(self, std, shape, seed=None):
        self.std = std
        self.shape = shape
        self.rg = np.random.default_rng(seed=seed)

    def __call__(self, x):
        noise = self.rg.normal(0, self.std, self.shape).astype(np.float32)
        return x + noise


def train_step(model, loss_fn, optimizer, lr_scheduler, data_loader):
    model.train()
    losses = []
    n = 0

    for inputs, labels in data_loader:
        preds = model(inputs)
        loss = loss_fn(preds, labels)

        # Zero accumulated gradients and perform backward pass
        optimizer.zero_grad()
        loss.backward()

        # Adjust weights
        optimizer.step()
        lr_scheduler.step()

        losses.append(loss.item())
        n += len(labels)

    return sum(losses) / n


def validation_step(model, loss_fn, data_loader):
    model.eval()
    losses = []
    n = 0
    for inputs, labels in data_loader:
        with torch.no_grad():
            preds = model(inputs)
            loss = loss_fn(preds, labels)
        losses.append(loss.item())
        n += len(labels)
    return sum(losses) / n


def top1_accuracy(model, test_inputs, test_labels) -> float:
    model.eval()
    with torch.no_grad():
        preds = model(test_inputs)
    pred_labels = preds.detach().argmax(dim=-1)
    return (pred_labels == test_labels).sum().numpy() / len(test_labels)


def main():
    model = nn.Sequential(
        nn.Linear(D_in, H), nn.ReLU(), nn.Linear(H, D_out), nn.Softmax(dim=1)
    )
    loss_fn = nn.CrossEntropyLoss()
    optimizer = torch.optim.Adam(
        model.parameters(), lr=learning_rate, weight_decay=0.01
    )
    lr_scheduler = torch.optim.lr_scheduler.ExponentialLR(optimizer, decay)

    transform = AdditiveGaussianNoise(std=0.2, shape=(D_in,), seed=0)
    train_data = PolynomialDataset(points=D_in, transform=transform)
    train_loader = DataLoader(
        train_data,
        batch_size=3,
        shuffle=True,  #
        # num_workers=2,
    )

    # test_data = PolynomialDataset(points=D_in, repeat=85, transform=transform)
    samples = generate_samples(seed=1234)
    test_data = list(islice(samples, 256))
    test_inputs = torch.stack([x for x, l in test_data])
    test_labels = torch.tensor([l for x, l in test_data])

    validation_loader = train_loader

    train_step_ = partial(
        train_step, model, loss_fn, optimizer, lr_scheduler, train_loader
    )

    fmt_str = "{:5d} {:.1f} {:7.3f} {:7.3f} {:.1f}"
    col_str = "{:>5} {:>4} {:>7} {:>7} {:>4}"
    print(col_str.format("epoch", "acc", "loss", "valloss", "lr"))

    for epoch in range(1000):
        loss = train_step_()

        if epoch % 100 != 0:
            continue

        test_accuracy = top1_accuracy(model, test_inputs, test_labels)
        validation_loss = validation_step(model, loss_fn, validation_loader)

        print(
            "{:5d} {:.1f} {:7.3f} {:7.3f} {:.1f}".format(
                epoch,
                100 * test_accuracy,
                loss,
                validation_loss,
                math.log(lr_scheduler.get_last_lr()[0]),
            )
        )


if __name__ == "__main__":
    main()


# TODO larger train set (not only 3 samples)
# TODO proper test/validation datasets
# TODO random_split + save the split
# TODO device? send tensors to GPU during iteration?
