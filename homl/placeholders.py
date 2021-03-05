import tensorflow as tf

x = tf.placeholder(tf.float32)

y = tf.placeholder(tf.float32)




res = x * y

feed_dict = {}
with tf.Session() as sess:
    feed_dict[x] = 3.
    feed_dict[y] = 2.
    result = sess.run(res,feed_dict=feed_dict)
    print(result)