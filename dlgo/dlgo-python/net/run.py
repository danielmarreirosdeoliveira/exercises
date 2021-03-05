import numpy as np
import load_mnist

# > X = np.array([[[288,289], [1.0, 0.0]], [[50,60], [0.0, 1.0]], [[70,120], [0.0, 1.0]]])
# > average_digit(X, 1)
# [60. 90.]
def average_digit(X, digit):
    filtered_data = [x[0] for x in X if np.argmax(x[1]) == digit]
    filtered_array = np.asarray(filtered_data)
    return np.average(filtered_array, axis=0)

train, test = load_mnist.load_data()

avg_eight = average_digit(train, 8)

x_3  = train[2][0]  # it's a  4
x_18 = train[17][0] # it's a  8

W = np.transpose(avg_eight)
# print(np.dot(W, x_3))         19.938224149634543
# print(np.dot(W, x_18))        54.10696465116223

def sigmoid_double(x):
    return 1.0 / (1.0 + np.exp(-x))

def sigmoid(z):
    return np.vectorize(sigmoid_double)(z)

def predict(x, W, b):
    return sigmoid_double(np.dot(W, x) + b)

b = -45
# print(predict(x_3, W, b))     1.3055966933479947e-11
# print(predict(x_18, W, b))    0.9998891214983433


# from matplotlib import pyplot as plt
#
# img = (np.reshape(avg_eight, (28, 28)))
# plt.imshow(img)
# plt.show()

def evaluate(X, digit, threshold, W, b):
    total_samples = 1.0 * len(X)
    correct_predictions = 0
    for x in X:
        if (predict(x[0], W, b) > threshold and np.argmax(x[1]) == digit):
            correct_predictions += 1
        if (predict(x[0], W, b) <= threshold and np.argmax(x[1]) != digit):
            correct_predictions += 1
    return correct_predictions / total_samples

print(evaluate(X=train, digit=8, threshold=0.5, W=W, b=b))
print(evaluate(X=test, digit=8, threshold=0.5, W=W, b=b))

eight_test = [x for x in test if np.argmax(x[1]) == 8]
print(evaluate(X=eight_test, digit=8, threshold=0.5, W=W, b=b))
