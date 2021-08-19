# https://blog.altoros.com/using-linear-regression-in-tensorflow.html

import sys
import numpy as np
import tensorflow as tf
import matplotlib.pyplot as plt


def normalize(array):
    return (array - array.mean()) / array.std()


samples = 100
error = np.random.rand(samples).astype(np.float32) * 0.01
x_data = np.random.rand(samples).astype(np.float32)
y_data = x_data * 0.1 + 0.3 + error

x_data_n = normalize(x_data)
y_data_n = normalize(y_data)

x_data_test = np.random.rand(samples).astype(np.float32)
y_data_test = x_data * 0.1 + 0.3

x_data_test_n = normalize(x_data)
y_data_test_n = normalize(y_data)

# TF graph input
X = tf.placeholder("float")
Y = tf.placeholder("float")

# Create a model
W = tf.Variable(np.random.randn(), name="weight")
b = tf.Variable(np.random.randn(), name="bias")
model = tf.add(tf.mul(X, W), b)

learning_rate = 0.1
training_iteration = 200

# Minimize squared errors (L2 loss)
cost_function = tf.reduce_sum(tf.pow(model - Y, 2)) / (2 * samples)
optimizer = tf.train.GradientDescentOptimizer(learning_rate).minimize(
    cost_function
)

init = tf.global_variables_initializer()

# Launch a graph
with tf.Session() as sess:
    sess.run(init)

    display_step = 20
    # Fit all training data
    for iteration in range(training_iteration):
        for (x, y) in zip(x_data_n, y_data_n):
            sess.run(optimizer, feed_dict={X: x, Y: y})

        # Display logs per iteration step
        if iteration % display_step == 0:
            print(
                "Iteration:",
                "%04d" % (iteration + 1),
                "cost=",
                "{:.9f}".format(
                    sess.run(
                        cost_function, feed_dict={X: x_data_n, Y: y_data_n}
                    )
                ),
                "W=",
                sess.run(W),
                "b=",
                sess.run(b),
            )

    tuning_cost = sess.run(
        cost_function,
        feed_dict={X: normalize(x_data_n), Y: normalize(y_data_n)},
    )

    print(
        "Tuning completed:",
        "cost=",
        "{:.9f}".format(tuning_cost),
        "W=",
        sess.run(W),
        "b=",
        sess.run(b),
    )

    # Validate a tuning model

    testing_cost = sess.run(
        cost_function, feed_dict={X: x_data_test_n, Y: y_data_test_n}
    )

    print("Testing data cost:", testing_cost)

    sys.stdout.flush()

    # Display a plot
    plt.figure()
    plt.plot(x_data_n, y_data_n, "ro", label="Normalized samples")
    plt.plot(
        x_data_test_n, y_data_test_n, "go", label="Normalized testing samples"
    )
    plt.plot(
        x_data_n, sess.run(W) * x_data_n + sess.run(b), label="Fitted line"
    )
    plt.legend()

    plt.show()
