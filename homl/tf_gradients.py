import tensorflow as tf



a = tf.constant(2.)
b = tf.constant(3.)
c = tf.constant(4.)

d = a * 3 + b * 4
e = c * 5 + d / 2

with tf.Session() as sess:
    print(sess.run(tf.gradients(e, [a,b,c,d,e])))