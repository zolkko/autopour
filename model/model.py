import numpy as np
import matplotlib.pyplot as plt
import skfuzzy as fuzzy

low_moistness_x = np.arange(0.0, 0.4, 0.1)
low_moistness_y = fuzzy.trapmf(low_moistness_x, [0, 0, 0.2, 0.3])

normal_moistness_x = np.arange(0.2, 0.7, 0.1)
normal_moistness_y = fuzzy.trapmf(normal_moistness_x, [0.2, 0.3, 0.5, 0.6])

plt.plot(low_moistness_x, low_moistness_y, color='g')
plt.plot(normal_moistness_x, normal_moistness_y, color='r')

plt.show()