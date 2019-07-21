from pprint import pprint
from typing import Tuple

from classification_models.resnet import ResNet18, preprocess_input
from keras.applications import imagenet_utils
from keras.preprocessing import image
# from keras_contrib.applications import ResNet18
import numpy as np
import tensorflow as tf
from tensorflow import keras

tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)

def create_model() -> keras.Model:
    # return ResNet18(input_shape=(224, 224, 3), classes=1000)
    return ResNet18(input_shape=(224, 224, 3), weights='imagenet')

def single_input_image(filename):
    img = image.load_img(filename, target_size=(224, 224))
    imgs = image.img_to_array(img)
    imgs = np.expand_dims(imgs, axis=0)
    return imagenet_utils.preprocess_input(imgs)

# def model_from_layers(layers) -> keras.Model:
#     input_layer = keras.Input(layers[0].input_shape[1:])
#     x = input_layer
#     for layer in layers:
#         print(layer)
#         x = layer(x)
#     return keras.Model(inputs=input_layer, outputs=x)

# TODO why are we recursing so many times? Is it because the for loop works on
# mutated nodes?

def copy_graph(layer, top_layers, input_layer, layer_lut):
    # lookup = layer_lut.get(layer, None)
    lookup = layer_lut.get(layer.name, None)
    if lookup is not None:
        return lookup

    print(layer)
    # pprint(layer_lut)

    # TODO removable
    if layer in top_layers:
        lookup = layer(input_layer)
        layer_lut[layer.name] = lookup
        return lookup

    # TODO are inbound nodes always singular?
    node = layer.inbound_nodes[0]

    if isinstance(node.inbound_layers, list):
        x = [
            copy_graph(x, top_layers, input_layer, layer_lut)
            for x in node.inbound_layers]
    else:
        x = copy_graph(node.inbound_layers, top_layers, input_layer, layer_lut)

    # print(x)
    lookup = layer(x)
    # layer_lut[layer] = lookup
    layer_lut[layer.name] = lookup
    return lookup


    # nodes = layer.outbound_nodes
    # if isinstance(nodes, list):
    #     [nodes]
    # x = input_layer
    # xs ?
    # x = input_layer
    # layer = top_layer
    # while layer != last_layer:
    #
    #     layer(x)  # ?

    # # print(layer)
    # if layer == top_layer:
    #     return layer(input_layer)
    # # for node in layer.inbound_nodes:
    # # print(layer.inbound_nodes)
    # # TODO why would there be more than one node?
    # node = layer.inbound_nodes[0]
    # print(node.inbound_layers)
    # # for node_layer in node.inbound_layers:
    # #     layers.append(...)
    # # print(node)
    # # print(node.inbound_layers)
    # if isinstance(node.inbound_layers, list):
    #     return layer([
    #         subgraph(x, top_layer, input_layer)
    #         for x in node.inbound_layers])
    #
    # x = node.inbound_layers
    # return layer(subgraph(x, top_layer, input_layer))

    # TODO this doesn't properly deal with MERGEs...
    # This just tries to search path backwards, but we need to account for merging!
    # How can we do that? Actually, since we can keep track of outbound nodes, why don't we do it in THAT order?
    # TODO also assert check that the corresponding layers are different after reconstruction

# TODO might be simpler to just rip the graph in two with "hacks" rather than
# doing this enumerate/loop over layers

# def model_from_layers(layers) -> keras.Model:
#     # TODO wat...
#     shape = layers[0].input_shape[1:]
#     if isinstance(shape, list):
#         assert len(shape) == 1
#         shape = shape[0]
#
#     input_layer = keras.Input(shape)
#     # outputs = get_output_of_layer(layers[-1], layers[0], input_layer, {})
#     outputs = copy_graph(layers[-1], layers[0], input_layer, {})
#     return keras.Model(inputs=input_layer, outputs=outputs)


def model_from_layers(layer, top_layer) -> keras.Model:
    shape = top_layer.input_shape
    shape = shape[0][1:] if isinstance(shape, list) else shape[1:]
    input_layer = keras.Input(shape)
    outputs = copy_graph(layer, [], input_layer, {top_layer.name: input_layer})
    return keras.Model(inputs=input_layer, outputs=outputs)

# def model_from_layers(layer, top_layers) -> keras.Model:
#     input_layer = keras.Input(top_layers[0].input_shape[1:])
#     outputs = copy_graph(layer, top_layers, input_layer, {})
#     return keras.Model(inputs=input_layer, outputs=outputs)
#
    # TODO call copy graph on
    # shape = layers[0].input_shape[1:]
    # if isinstance(shape, list):
    #     assert len(shape) == 1
    #     shape = shape[0]
    #
    # input_layer = keras.Input(shape)
    # # outputs = get_output_of_layer(layers[-1], layers[0], input_layer, {})
    # outputs = copy_graph(layers[-1], layers[0], input_layer, {})
    # return keras.Model(inputs=input_layer, outputs=outputs)

def split_model(
    model: keras.Model,
    split_idx: int
) -> Tuple[keras.Model, keras.Model]:
    # TODO model1 = range([input_of_1,          output_of_split_idx])
    # TODO model2 = range([output_of_split_idx, output_of_end])
    # model1 = model_from_layers(model.layers[1:split_idx+1])  # TODO + 1?
    # model2 = model_from_layers(model.layers[split_idx+1:])
    layers = model.layers
    split_layer = layers[split_idx]
    print(split_layer.outbound_nodes)
    print([x.outbound_layer.name for x in split_layer.outbound_nodes])
    # model1 = model_from_layers(split_layer, [layers[1]])
    # model2 = model_from_layers(
    #     layers[-1], [x.outbound_layer for x in split_layer.outbound_nodes])
    model1 = model_from_layers(split_layer, layers[0])
    model2 = model_from_layers(layers[-1], split_layer)
    return model1, model2

def get_layer_idx_by_name(model: keras.Model, name: str) -> int:
    return next(i for i, layer in enumerate(model.layers) if layer.name == name)

# # TODO make this an actually proper algorithm...
# def get_output_of_layer(layer, top_layer, input_layer, layer_outputs):
#     print(layer)
#     # if we have already applied this layer on its input(s) tensors,
#     # just return its already computed output
#     if layer.name in layer_outputs:
#         return layer_outputs[layer.name]
#
#     # if this is the starting layer, then apply it on the input tensor
#     if layer.name == top_layer:
#         out = layer(input_layer)
#         layer_outputs[layer.name] = out
#         return out
#
#     # find all the connected layers which this layer
#     # consumes their output
#     prev_layers = []
#     for node in layer._inbound_nodes:
#         print(node)
#         prev_layers.extend(node.inbound_layers)
#
#     # get the output of connected layers
#     pl_outs = []
#     for pl in prev_layers:
#         pl_outs.extend([get_output_of_layer(pl, top_layer, input_layer, layer_outputs)])
#
#     # apply this layer on the collected outputs
#     out = layer(pl_outs[0] if len(pl_outs) == 1 else pl_outs)
#     layer_outputs[layer.name] = out
#     return out

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
