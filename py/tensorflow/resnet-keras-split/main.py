from pprint import pprint
from typing import Dict, Tuple

from classification_models.resnet import (
    preprocess_input, ResNet18, ResNet34, ResNet50, ResNet101, ResNet152)
import numpy as np
import tensorflow as tf
from tensorflow import keras
# pylint: disable-msg=E0611
from tensorflow.python.keras.applications import imagenet_utils
from tensorflow.python.keras.preprocessing import image
from tensorflow.python.keras.utils import plot_model
from tensorflow.python.keras.layers import Layer
import tensorflow.python.keras.backend as K
#pylint: enable-msg=E0611

tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)

class EncoderLayer(Layer):
    def __init__(self, clip_range, **kwargs):
        self.clip_range = clip_range
        super(EncoderLayer, self).__init__(**kwargs)

    def call(self, x):
        # x = K.log(x)
        # x = K.clip(x, -4, 1)
        # x = (x + 4) * (255 / 5)
        scale = 255 / (self.clip_range[1] - self.clip_range[0])
        x = (x - self.clip_range[0]) * scale
        x = K.cast(x, 'uint8')
        return x

    def get_config(self):
        config = {'clip_range': self.clip_range}
        config.update(super(EncoderLayer, self).get_config())
        return config

class DecoderLayer(Layer):
    def __init__(self, clip_range, **kwargs):
        self.clip_range = clip_range
        super(DecoderLayer, self).__init__(**kwargs)

    def call(self, x):
        scale = (self.clip_range[1] - self.clip_range[0]) / 255
        x = K.cast(x, 'float32')
        x = x * scale + self.clip_range[0]
        return x

    def get_config(self):
        config = {'clip_range': self.clip_range}
        config.update(super(DecoderLayer, self).get_config())
        return config

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

def copy_graph(layer: Layer, layer_lut: Dict[Layer, Layer]) -> Layer:
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

# def model_from_layers(layer, top_layer) -> keras.Model:
#     shape = top_layer.input_shape
#     shape = shape[0][1:] if isinstance(shape, list) else shape[1:]
#     input_layer = keras.Input(shape)
#     outputs = copy_graph(layer, {top_layer.name: input_layer})
#     return keras.Model(inputs=input_layer, outputs=outputs)

def input_shape(layer: Layer) -> Tuple[int, ...]:
    shape = layer.input_shape
    return shape[0][1:] if isinstance(shape, list) else shape[1:]

def model_from_layers(layer, top_layer) -> keras.Model:
    input_layer = keras.Input(input_shape(top_layer))
    outputs = copy_graph(layer, {top_layer.name: input_layer})
    return keras.Model(inputs=input_layer, outputs=outputs)

def split_model(
    model: keras.Model,
    split_idx: int
) -> Tuple[keras.Model, keras.Model]:
    clip_range = (-2, 2)
    layers = model.layers
    first_layer = layers[0]
    split_layer = layers[split_idx]

    input_layer1 = keras.Input(input_shape(first_layer))
    outputs1 = copy_graph(split_layer, {first_layer.name: input_layer1})
    outputs1 = EncoderLayer(clip_range)(outputs1)
    model1 = keras.Model(inputs=input_layer1, outputs=outputs1)
    # model1 = model_from_layers(split_layer, layers[0])

    input_layer2 = keras.Input(input_shape(split_layer), dtype='uint8')
    top_layer2 = DecoderLayer(clip_range)(input_layer2)
    outputs2 = copy_graph(layers[-1], {split_layer.name: top_layer2})
    model2 = keras.Model(inputs=input_layer2, outputs=outputs2)
    # model2 = model_from_layers(layers[-1], split_layer)

    return model1, model2

def get_layer_idx_by_name(model: keras.Model, name: str) -> int:
    return next(i for i, layer in enumerate(model.layers) if layer.name == name)

def write_summary_to_file(model: keras.Model, filename: str):
    with open(filename, 'w') as f:
        model.summary(print_fn=lambda x: f.write(f'{x}\n'))

def write_tflite_model(sess, model: keras.Model, filename: str):
    converter = tf.lite.TFLiteConverter.from_session(
        sess, model.inputs, model.outputs)
    tflite_model = converter.convert()
    with open(filename, 'wb') as f:
        f.write(tflite_model)

def convert_tflite_model(keras_model_filename, tflite_filename, **kwargs):
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        keras_model_filename, **kwargs)
    tflite_model = converter.convert()
    with open(tflite_filename, 'wb') as f:
        f.write(tflite_model)

def main():
    model_name = 'resnet34'
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
        model_client = keras.models.load_model(
            f'{prefix}-client.h5',
            custom_objects={'EncoderLayer': EncoderLayer})
        model_server = keras.models.load_model(
            f'{prefix}-server.h5',
            custom_objects={'DecoderLayer': DecoderLayer})
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

    # Save TFLite models
    convert_tflite_model(
        f'{prefix}-full.h5',
        f'{prefix}-full.tflite')

    convert_tflite_model(
        f'{prefix}-client.h5',
        f'{prefix}-client.tflite',
        custom_objects={'EncoderLayer': EncoderLayer})

    # sess = K.get_session()
    # write_tflite_model(sess, model, f'{prefix}-full.tflite')
    # write_tflite_model(sess, model_client, f'{prefix}-client.tflite')

if __name__ == "__main__":
    main()
