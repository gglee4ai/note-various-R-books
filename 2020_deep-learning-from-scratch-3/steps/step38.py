if '__file__' in globals():
    import os, sys
    sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import numpy as np
from dezero import Variable
import dezero.functions as F

x = Variable(np.array([[1, 2, 3], [4, 5, 6]]))
y = F.reshape(x, (6,))
y.backward(retain_grad=True)
print(x.grad)

print('y:', y)

print('-' * 12)
x = Variable(np.random.randn(1, 2, 3))
y = x.reshape((2, 3))
y = x.reshape(2, 3)

print(y)




# transpose test

y = F.transpose(x)
y.backward()
print("x.grad", x.grad)



x = Variable(np.random.rand(2,30))
y = x.transpose()
y = x.T
y.backward(retain_grad=True)
print(y)
print(x)
print(x.grad)
