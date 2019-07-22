from pprint import pprint
from typing import Tuple

from classification_models.resnet import (
    preprocess_input, ResNet18, ResNet34, ResNet50, ResNet101, ResNet152)
import numpy as np
import tensorflow as tf
from tensorflow import keras
# pylint: disable-msg=E0611
from tensorflow.python.keras.applications import imagenet_utils
from tensorflow.python.keras.preprocessing import image
from tensorflow.python.keras.utils import plot_model
#pylint: enable-msg=E0611

tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)

def create_model(model_name) -> keras.Model:
    shape = (224, 224, 3)
    d = {
        'resnet18':  lambda: ResNet18(input_shape=shape, weights='imagenet'),
        'resnet34':  lambda: ResNet34(input_shape=shape, weights='imagenet'),
        'resnet50':  lambda: ResNet50(input_shape=shape, weights='imagenet'),
        'resnet101': lambda: ResNet101(input_shape=shape, weights='imagenet'),
        'resnet152': lambda: ResNet152(input_shape=shape, weights='imagenet'),
    }
    return d[model_name]()

def single_input_image(filename):
    img = image.load_img(filename, target_size=(224, 224))
    imgs = image.img_to_array(img)
    imgs = np.expand_dims(imgs, axis=0)
    return preprocess_input(imgs)

def copy_graph(layer, layer_lut):
    lookup = layer_lut.get(layer.name, None)
    if lookup is not None:
        return lookup

    inbound_layers = layer.inbound_nodes[0].inbound_layers

    x = (
        [copy_graph(x, layer_lut) for x in inbound_layers]
        if isinstance(inbound_layers, list) else
        copy_graph(inbound_layers, layer_lut))

    lookup = layer(x)
    layer_lut[layer.name] = lookup
    return lookup

def model_from_layers(layer, top_layer) -> keras.Model:
    shape = top_layer.input_shape
    shape = shape[0][1:] if isinstance(shape, list) else shape[1:]
    input_layer = keras.Input(shape)
    outputs = copy_graph(layer, {top_layer.name: input_layer})
    return keras.Model(inputs=input_layer, outputs=outputs)

def split_model(
    model: keras.Model,
    split_idx: int
) -> Tuple[keras.Model, keras.Model]:
    layers = model.layers
    split_layer = layers[split_idx]
    model1 = model_from_layers(split_layer, layers[0])
    model2 = model_from_layers(layers[-1], split_layer)
    return model1, model2

def get_layer_idx_by_name(model: keras.Model, name: str) -> int:
    return next(i for i, layer in enumerate(model.layers) if layer.name == name)

def write_summary_to_file(model: keras.Model, filename: str):
    with open(filename, 'w') as f:
        model.summary(print_fn=lambda x: f.write(f'{x}\n'))

def main():
    model_name = 'resnet18'
    prefix = f'{model_name}/{model_name}'
    split_layer_name = {
        'resnet18':  'add_5',   # (14, 14, 256)
        # 'resnet18':  'add_7', # (7, 7, 512)
        'resnet34':  'add_8',   # (14, 14, 256)
        'resnet50':  'add_8',   # (14, 14, 1024)
        'resnet101': 'add_8',   # (14, 14, 1024)
        'resnet152': 'add_12',  # (14, 14, 1024)
    }[model_name]

    test_images = single_input_image(
        '/home/mulhaq/code/downloaded/datasets/imagenette/train/n02102040/'
        'n02102040_334.JPEG')

    # Load and save entire model
    try:
        model = keras.models.load_model(f'{prefix}-full.h5')
    except OSError:
        model = create_model(model_name)
        # model.load_weights(f'{prefix}_imagenet_1000.h5')  # Unnecessary
        plot_model(model, to_file=f'{prefix}-full.png')
        model.save(f'{prefix}-full.h5')

    # Force usage of tf.keras.Model, which appears to have Nodes linked correctly
    model = keras.models.load_model(f'{prefix}-full.h5')

    # Make predictions
    predictions = model.predict(test_images)
    predictions = imagenet_utils.decode_predictions(predictions)
    write_summary_to_file(model, f'{prefix}-full.txt')
    pprint(predictions)

    # Load and save split model
    try:
        model_client = keras.models.load_model(f'{prefix}-client.h5')
        model_server = keras.models.load_model(f'{prefix}-server.h5')
    except OSError:
        model_client, model_server = split_model(
            model, get_layer_idx_by_name(model, split_layer_name))
        model_client.save(f'{prefix}-client.h5')
        model_server.save(f'{prefix}-server.h5')
        plot_model(model_client, to_file=f'{prefix}-client.png')
        plot_model(model_server, to_file=f'{prefix}-server.png')

    # Make predictions
    prev_predictions = predictions
    predictions = model_client.predict(test_images)
    predictions = model_server.predict(predictions)
    predictions = imagenet_utils.decode_predictions(predictions)
    write_summary_to_file(model, f'{prefix}-client.txt')
    write_summary_to_file(model, f'{prefix}-server.txt')
    pprint(predictions)
    print('Same predictions with split model? '
          f'{np.all(predictions == prev_predictions)}')

    # Save model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        f'{prefix}-full.h5')
    tflite_model = converter.convert()
    with open(f'{prefix}-full.tflite', 'wb') as f:
        f.write(tflite_model)

    # Save client model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        f'{prefix}-client.h5')
    tflite_model = converter.convert()
    with open(f'{prefix}-client.tflite', 'wb') as f:
        f.write(tflite_model)

if __name__ == "__main__":
    main()
