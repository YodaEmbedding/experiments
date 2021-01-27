from typing import Any, List, Type, Union

import torch
import torch.nn.functional
from keras.applications.imagenet_utils import decode_predictions
from PIL import Image
from torch.functional import Tensor
from torchvision import transforms
from torchvision.models.resnet import (
    BasicBlock,
    Bottleneck,
    ResNet,
    model_urls,
)
from torchvision.models.utils import load_state_dict_from_url

DATASET = "/mnt/data/datasets/ILSVRC2012/validation"


class SplittableResNet(ResNet):
    def __init__(self, split_idx, is_client, *args, **kwargs):
        self._split_idx = split_idx
        self._is_client = is_client
        super().__init__(*args, **kwargs)

    def _forward_impl(self, x: Tensor) -> Tensor:
        layers = [
            self.layer1,
            self.layer2,
            self.layer3,
            self.layer4,
        ]

        if self._is_client:
            x = self.conv1(x)
            x = self.bn1(x)
            x = self.relu(x)
            x = self.maxpool(x)

            for layer in layers[: self._split_idx]:
                x = layer(x)

            return x
        else:
            for layer in layers[self._split_idx :]:
                x = layer(x)

            x = self.avgpool(x)
            x = torch.flatten(x, 1)
            x = self.fc(x)

            return x

    def forward(self, x: Tensor) -> Tensor:
        return self._forward_impl(x)


def _resnet(
    arch: str,
    block: Type[Union[BasicBlock, Bottleneck]],
    layers: List[int],
    pretrained: bool,
    progress: bool,
    **kwargs: Any
) -> SplittableResNet:
    model = SplittableResNet(block=block, layers=layers, **kwargs)
    if pretrained:
        state_dict = load_state_dict_from_url(
            model_urls[arch],
            progress=progress,
        )
        model.load_state_dict(state_dict)
    return model


def resnet50(
    pretrained: bool = False, progress: bool = True, **kwargs: Any
) -> SplittableResNet:
    return _resnet(
        "resnet50", Bottleneck, [3, 4, 6, 3], pretrained, progress, **kwargs
    )



def main():
    model_client = resnet50(pretrained=True, split_idx=1, is_client=True)
    model_server = resnet50(pretrained=True, split_idx=1, is_client=False)

    img = Image.open("sample_224x224.jpg")

    preprocess = transforms.Compose(
        [
            transforms.Resize(256),
            transforms.CenterCrop(224),
            transforms.ToTensor(),
            transforms.Normalize(
                mean=[0.485, 0.456, 0.406],
                std=[0.229, 0.224, 0.225],
            ),
        ]
    )

    x = preprocess(img).unsqueeze(0)

    model_client.eval()
    model_server.eval()

    with torch.no_grad():
        features = model_client(x)
        preds = model_server(features)
        preds = torch.nn.functional.softmax(preds, dim=-1)
        preds = preds.detach().numpy()

    print(decode_predictions(preds))


main()
