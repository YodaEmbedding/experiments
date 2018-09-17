#!/usr/bin/env python3

import itertools
import threading

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torchvision import datasets, transforms

class Net(nn.Module):
    def __init__(self):
        super().__init__()
        self.conv1 = nn.Conv2d(1, 10, kernel_size=5)
        self.conv2 = nn.Conv2d(10, 25, kernel_size=5)
        self.conv2_drop = nn.Dropout2d()
        self.fc1 = nn.Linear(400, 50)
        self.fc2 = nn.Linear(50, 10)

    def forward(self, x):
        x = self.conv1(x)
        x = F.relu(F.max_pool2d(x, 2))
        x = self.conv2(x)
        x = self.conv2_drop(x)
        x = F.relu(F.max_pool2d(x, 2))
        x = x.view(-1, 400)
        x = self.fc1(x)
        x = F.relu(x)
        x = F.dropout(x)  # TODO training=self.training?
        x = self.fc2(x)
        return F.log_softmax(x, dim=1)

class LabelledDataset(torch.utils.data.Dataset):
    def __init__(self, data, labels):
        self.data = data
        self.labels = labels
        self.length = self.data.shape[0]

    def __len__(self):
        return self.length

    def __getitem__(self, idx):
        return self.data[idx], self.labels[idx]

def one_hot_encode(x, length):
    n = x.shape[0]
    arr = np.zeros((n, length), dtype=np.float32)
    arr[np.arange(n), x] = 1.0
    return arr

def load(filename):
    df = pd.read_csv(filename)
    data = (df.drop(labels=["label"], axis=1)
        .values
        .reshape(-1, 1, 28, 28)
        .astype(dtype=np.float32)) / 255.0
    labels = df["label"]
    # labels = one_hot_encode(labels, 10)
    return data, labels

def train(model, device, loader, optimizer):
    model.train()
    total_loss = 0.0

    for batch_idx, (data, target) in enumerate(loader):
        data, target = data.to(device), target.to(device)
        optimizer.zero_grad()
        output = model(data)
        loss = F.nll_loss(output, target)
        loss.backward()
        optimizer.step()
        total_loss += loss.item()

    return total_loss / (batch_idx + 1)

def main():
    data, labels = load('train.csv')
    dataset = LabelledDataset(data, labels)
    train_len = int(0.80 * len(dataset))
    test_len  = len(dataset) - train_len
    train_dataset, test_dataset = torch.utils.data.random_split(dataset,
        [train_len, test_len])

    train_loader = torch.utils.data.DataLoader(train_dataset,
        batch_size=64, shuffle=True, num_workers=2, pin_memory=True)
    test_loader = torch.utils.data.DataLoader(test_dataset,
        batch_size=64, shuffle=True, num_workers=2, pin_memory=True)

    device = torch.device("cuda")
    model = Net().to(device)
    model.load_state_dict(torch.load('checkpoint.pt'))
    optimizer = optim.SGD(model.parameters(), lr=0.01, momentum=0.5)

    queue = []
    threading.Thread(target=lambda q: q.append(input()), args=(queue,)).start()

    for epoch in itertools.count():
        print(f'Epoch: {epoch}  ', end='')
        loss = train(model, device, test_loader, optimizer)
        print(f'Loss: {loss:.4f}')

        if len(queue) > 0:
            break

    torch.save(model.state_dict(), 'checkpoint.pt')

if __name__ == "__main__":
    main()
