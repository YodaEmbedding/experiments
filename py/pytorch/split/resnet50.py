import os

import numpy as np
import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader
from torchvision.models.resnet import resnet50

DATASET = "/mnt/data/datasets/ILSVRC2012/validation"

# CIL
#


class MyDataset(Dataset):
    def __init__(self, points, transform=None):
        # for ....
        pass

        # os....

        # see also FolderDataset/etc code...

        # And other things

        # t = np.linspace(0, 1, points, dtype=np.float32)
        # self.t = t
        # self.samples = [
        #     0.5 * np.ones_like(t),
        #     t,
        #     t ** 2,  #
        # ]
        # self.labels = [0, 1, 2]
        # identity = lambda x: x
        # self.transform = identity if transform is None else transform

    def __getitem__(self, index):
        pass
        # sample = self.transform(self.samples[index])
        # label = self.labels[index]
        # return sample, label

    def __len__(self):
        pass
        # return len(self.samples)


model = resnet50(pretrained=True)
model.eval()


# model()

# ...




