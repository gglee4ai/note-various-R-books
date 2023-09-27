class Variabel:
    def __init__(self, data):
        self.data = data

import numpy as np
data = np.array(1.0)
x = Variabel(data)
print(x.data)
