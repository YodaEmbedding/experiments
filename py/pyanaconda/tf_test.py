import os
import sys
import tensorflow as tf

X = 1
W = 1
b = 1

tf.nn.softmax(tf.matmul(X, W) + b)

