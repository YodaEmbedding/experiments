import numpy as np
import tensorflow as tf
from tensorflow import keras

tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)


def create_model():
    model = keras.Sequential(
        [
            keras.layers.Flatten(input_shape=(28, 28)),
            keras.layers.Dense(128, activation=tf.nn.relu),
            keras.layers.Dense(10, activation=tf.nn.softmax),
        ]
    )
    return model


def compile_model(model):
    model.compile(
        optimizer="adam",
        loss="sparse_categorical_crossentropy",
        metrics=["accuracy"],
    )


def split_model(model, split_idx):
    model1 = keras.Sequential(model.layers[:split_idx])
    model2 = keras.Sequential(model.layers[split_idx:])
    model1.build(model1.layers[0].input_shape)
    model2.build(model2.layers[0].input_shape)
    return model1, model2


def main():
    fashion_mnist = keras.datasets.fashion_mnist
    (train_images, train_labels), (
        test_images,
        test_labels,
    ) = fashion_mnist.load_data()

    train_images = train_images / 255.0
    test_images = test_images / 255.0

    # Load and save entire model
    try:
        model = keras.models.load_model("mnist-fashion-full.h5")
    except OSError:
        model = create_model()
        compile_model(model)
        model.fit(train_images, train_labels, epochs=5)

        # Strip unnecessary metadata and save
        model = keras.Sequential(model.layers)
        model.build(model.layers[0].input_shape)
        model.save("mnist-fashion-full.h5")

    # Make predictions
    predictions = model.predict(test_images)
    predictions = np.argmax(predictions, axis=1)
    model.summary()
    print(predictions)

    # Load and save split model
    try:
        model_client = keras.models.load_model("mnist-fashion-client.h5")
        model_server = keras.models.load_model("mnist-fashion-server.h5")
    except OSError:
        model_client, model_server = split_model(model, 2)
        model_client.save("mnist-fashion-client.h5")
        model_server.save("mnist-fashion-server.h5")

    # Make predictions
    prev_predictions = predictions
    predictions = model_client.predict(test_images)
    predictions = model_server.predict(predictions)
    predictions = np.argmax(predictions, axis=1)
    model_client.summary()
    model_server.summary()
    print(predictions)
    print(
        "Same predictions with split model? "
        f"{np.all(predictions == prev_predictions)}"
    )

    # Save client model as tflite model
    converter = tf.lite.TFLiteConverter.from_keras_model_file(
        "mnist-fashion-client.h5"
    )
    tflite_model = converter.convert()
    open("mnist-fashion-client.tflite", "wb").write(tflite_model)


main()
