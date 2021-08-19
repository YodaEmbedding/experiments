from pprint import pprint
from typing import Tuple

import numpy as np
import tensorflow as tf
from tensorflow import keras
from keras.applications import imagenet_utils
from keras.preprocessing import image

tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)


def create_model() -> keras.Model:
    model = keras.applications.mobilenet.MobileNet(
        input_shape=None,
        alpha=1.0,
        depth_multiplier=1,
        dropout=1e-3,
        include_top=True,
        weights="imagenet",
        input_tensor=None,
        pooling=None,
        classes=1000,
    )
    return model


def single_input_image(filename):
    img = image.load_img(filename, target_size=(224, 224))
    imgs = image.img_to_array(img)
    imgs = np.expand_dims(imgs, axis=0)
    return keras.applications.mobilenet.preprocess_input(imgs)


def model_from_layers(layers):
    input_layer = keras.Input(layers[0].input_shape[1:])
    model = input_layer
    for layer in layers:
        model = layer(model)
    model = keras.Model(inputs=input_layer, outputs=model)
    return model


def split_model(
    model: keras.Model, split_idx: int
) -> Tuple[keras.Model, keras.Model]:
    model1 = model_from_layers(model.layers[1:split_idx])
    model2 = model_from_layers(model.layers[split_idx:])
    return model1, model2


def get_layer_idx_by_name(model: keras.Model, name: str) -> int:
    return next(
        i for i, layer in enumerate(model.layers) if layer.name == name
    )


def main():
    model_name = "mobilenet_v1_1.0_224"

    test_images = single_input_image(
        "/home/mulhaq/code/downloaded/datasets/imagenette/train/n02102040/"
        "n02102040_334.JPEG"
    )

    # Load and save entire model
    try:
        model = keras.models.load_model(f"{model_name}-full.h5")
    except OSError:
        model = create_model()
        model.save(f"{model_name}-full.h5")

    # Make predictions
    predictions = model.predict(test_images)
    predictions = imagenet_utils.decode_predictions(predictions)
    model.summary()
    pprint(predictions)

    # Load and save split model
    try:
        model_client = keras.models.load_model(f"{model_name}-client.h5")
        model_server = keras.models.load_model(f"{model_name}-server.h5")
    except OSError:
        model_client, model_server = split_model(
            model, get_layer_idx_by_name(model, "conv_pw_12")
        )
        model_client.save(f"{model_name}-client.h5")
        model_server.save(f"{model_name}-server.h5")

    # Make predictions
    prev_predictions = predictions
    predictions = model_client.predict(test_images)
    predictions = model_server.predict(predictions)
    predictions = imagenet_utils.decode_predictions(predictions)
    model_client.summary()
    model_server.summary()
    pprint(predictions)
    print(
        "Same predictions with split model? "
        f"{np.all(predictions == prev_predictions)}"
    )

    # Save model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        f"{model_name}-full.h5"
    )
    tflite_model = converter.convert()
    open(f"{model_name}-full.tflite", "wb").write(tflite_model)

    # Save client model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        f"{model_name}-client.h5"
    )
    tflite_model = converter.convert()
    open(f"{model_name}-client.tflite", "wb").write(tflite_model)


main()
