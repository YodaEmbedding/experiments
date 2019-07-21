from pprint import pprint
from typing import Tuple

from classification_models.resnet import ResNet18, preprocess_input
from keras.applications import imagenet_utils
from keras.preprocessing import image
import numpy as np
import tensorflow as tf
from tensorflow import keras

tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)

def create_model() -> keras.Model:
    return ResNet18(input_shape=(224, 224, 3), weights='imagenet')

def single_input_image(filename):
    img = image.load_img(filename, target_size=(224, 224))
    imgs = image.img_to_array(img)
    imgs = np.expand_dims(imgs, axis=0)
    return imagenet_utils.preprocess_input(imgs)

def copy_graph(layer, layer_lut):
    lookup = layer_lut.get(layer.name, None)
    if lookup is not None:
        return lookup

    # TODO is there always only one inbound node?
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

def main():
    model_name = 'resnet18'

    test_images = single_input_image(
        '/home/mulhaq/code/downloaded/datasets/imagenette/train/n02102040/'
        'n02102040_334.JPEG')

    # Load and save entire model
    try:
        model = keras.models.load_model(f'{model_name}-full.h5')
    except OSError:
        model = create_model()
        model.summary()
        model.load_weights(f'{model_name}.h5')
        model.save(f'{model_name}-full.h5')

    # Force usage of tf.keras.Model, which appears to have Nodes linked correctly
    model = keras.models.load_model(f'{model_name}-full.h5')

    # Make predictions
    predictions = model.predict(test_images)
    predictions = imagenet_utils.decode_predictions(predictions)
    model.summary()
    pprint(predictions)

    # Load and save split model
    try:
        model_client = keras.models.load_model(f'{model_name}-client.h5')
        model_server = keras.models.load_model(f'{model_name}-server.h5')
    except OSError:
        model_client, model_server = split_model(
            model, get_layer_idx_by_name(model, 'add_7'))
        model_client.save(f'{model_name}-client.h5')
        model_server.save(f'{model_name}-server.h5')

    # Make predictions
    prev_predictions = predictions
    predictions = model_client.predict(test_images)
    predictions = model_server.predict(predictions)
    predictions = imagenet_utils.decode_predictions(predictions)
    model_client.summary()
    model_server.summary()
    pprint(predictions)
    print('Same predictions with split model? '
          f'{np.all(predictions == prev_predictions)}')

    # Save model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        f'{model_name}-full.h5')
    tflite_model = converter.convert()
    open(f'{model_name}-full.tflite', 'wb').write(tflite_model)

    # Save client model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        f'{model_name}-client.h5')
    tflite_model = converter.convert()
    open(f'{model_name}-client.tflite', 'wb').write(tflite_model)

if __name__ == "__main__":
    main()
