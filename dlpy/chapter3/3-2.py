import numpy as np

input = np.array([[1, 2, 7, 8], [3, 0, 9]])
results = np.zeros((2, 10))

for i, sequence in enumerate(input):
    results[i, sequence] = 1

print(results)
