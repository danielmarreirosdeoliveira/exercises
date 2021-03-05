import matplotlib.pyplot as plt

with open('running-averages') as f:
    lst = [x for x in next(f).split()]
    lst = [float(i) for i in lst]

plt.scatter([i for i in range(len(lst))], lst)
plt.ylabel('running averages')
plt.show()