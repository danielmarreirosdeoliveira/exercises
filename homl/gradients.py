


import tensorflow as tf
import numpy as np


import tensorflow as tf
from datetime import datetime
now = datetime.utcnow().strftime("%Y%m%d%H%M%S")
root_logdir = "tf_logs"
logdir = "{}/run-{}/".format(root_logdir, now)
tf.reset_default_graph()

tf.set_random_seed(42)
np.random.seed(42)

X = tf.placeholder(tf.float32, shape=[None, 2])
hidden = tf.layers.dense(X, 2, activation=tf.nn.elu, kernel_initializer=tf.variance_scaling_initializer())
logits = tf.layers.dense(hidden, 1)
loss = 10 - logits * 4

optimizer = tf.train.AdamOptimizer(0.01)

gradient_placeholders = []

grads_and_vars = optimizer.compute_gradients(loss)

grads = [grad for grad, vari in grads_and_vars]

file_writer = tf.summary.FileWriter(logdir, tf.get_default_graph())

for grad, variable in grads_and_vars:
    print(grad)

with tf.Session() as sess:
    tf.global_variables_initializer().run()
    g_vals = sess.run(grads, feed_dict={X: [[1., 2.]]})
    print(g_vals[0])