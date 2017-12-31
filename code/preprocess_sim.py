import sys
import math
import pandas as pd
import numpy as np
import cPickle as pkl

folder= sys.argv[-1] # 'treated'

import statsmodels.api as sm
from statsmodels.tsa.arima_process import arma_generate_sample

import itertools

# Simulate time series using ARMA model 

analysis = sys.argv[-1] # 'treated' or 'control'
dataname = sys.argv[-2]
print('Generating {} data for analysis on {}'.format(dataname, analysis))

n_post  = 5
n_pre = 47-(n_post*2)
seq_len = 47

nb_features = 500
output_dim = 5

arparams = np.array([.75, -.25])
maparams = np.array([.65, .35])
arparams = np.r_[1, -arparams] # add zero-lag and negate
maparam = np.r_[1, maparams] # add zero-lag

x = arma_generate_sample(arparams, maparams, seq_len+n_post)

for _ in itertools.repeat(None, nb_features-1):
    x = np.c_[x, arma_generate_sample(arparams, maparams, seq_len+n_post)]

X_train = x[:seq_len]

X_test = x[seq_len:]

y = arma_generate_sample(arparams, maparams, seq_len+n_post)

for _ in itertools.repeat(None, output_dim-1):
    y = np.c_[y, arma_generate_sample(arparams, maparams, seq_len+n_post)]

y_train = y[:seq_len]

y_test = y[seq_len:] - abs(y[seq_len:]*0.1) 

y_train = np.reshape(y_train, (y_train.shape[0], output_dim))

print('X_train shape:', X_train.shape)

print('X_test shape:', X_test.shape)

print('y_train shape:', y_train.shape)

print('y_test shape:', y_test.shape)

## Save train and test sets to disk
print('Saving simulation data')

pkl.dump(X_train, open('data/sim_x_train_treated.np', 'wb')) 
pkl.dump(X_test, open('data/sim_x_test_treated.np', 'wb'))

pkl.dump(y_train, open('data/sim_y_train_treated.np', 'wb'))
pkl.dump(y_test, open('data/sim_y_test_treated.np', 'wb'))

np.savetxt('data/sim_x_train_treated.csv', X_train, delimiter=",")
np.savetxt('data/sim_x_test_treated.csv', X_test, delimiter=",")

np.savetxt('data/sim_y_train_treated.csv', y_train, delimiter=",")
np.savetxt('data/sim_y_test_treated.csv', y_test, delimiter=",")