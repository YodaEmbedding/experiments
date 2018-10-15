import numpy as np
import tensorflow as tf
import tensorflow.keras as keras
import tensorflow.keras.backend as K

Dense = tf.keras.layers.Dense
Sequential = tf.keras.models.Sequential

# TODO eager execution vs static graph...

# TODO generalize
X_MIN = 0.0
X_MAX = 2*np.pi
N = 256

def integrate(f, a, b):
    x = tf.lin_space(a, b, N)
    y = f(x)
    return K.sum(y)

def f_model(w, x):
    return w[0] * K.sin(x)

def loss(y_true, y_pred):
    f = lambda x: K.abs(f_model(y_true, x) - f_model(y_pred, x))
    return integrate(f, X_MIN, X_MAX)

labels = np.array([0.1])
x = np.linspace(X_MIN, X_MAX, N)
inputs = np.array([0.1 * np.sin(x)])

model = Sequential()
model.add(Dense(units=64, activation='relu'))
model.add(Dense(units=32, activation='relu'))
model.add(Dense(units=1, activation='linear'))

model.compile(
    loss=loss,
    optimizer='sgd',
    metrics=['accuracy'])

model.fit(inputs, labels, epochs=5, batch_size=1)  # batch size of 1 for maximum kappa

scores = model.evaluate(inputs, labels)
preds = model.predict(inputs)

print()
print('\n'.join(f'{name}: {score}'
    for name, score in zip(model.metrics_names, scores)))
print(preds)
