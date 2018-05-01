import sys 
import matplotlib.pyplot as plt
import numpy as np

# Read training log
history = np.genfromtxt(sys.argv[-2], names=True, delimiter=",")

# Summarize history for accuracy
plt.plot(history['loss'])
plt.plot(history['val_loss'])
plt.title(sys.argv[-1])
plt.ylabel('Mean squared prediction error (MSPE)')
plt.xlabel('Epoch')
plt.legend(['Training', 'Validation'], loc='upper right')
plt.show()