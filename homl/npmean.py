




import numpy as np



allgs = []
allgs.append(np.array([[2.,1.], [3.,5.]]))
allgs.append(np.array([[3.,1.], [3.,5.]]))




print([a for a in allgs])
print(np.mean(allgs, axis=0))

