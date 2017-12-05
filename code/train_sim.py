from __future__ import print_function

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import sys
import math
import numpy as np
import cPickle as pkl
import pandas as pd

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Dense, Permute, Reshape, Input, merge, Masking, RepeatVector, TimeDistributed, Dropout
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras import regularizers
from keras.optimizers import Adam

from attention_utils import get_activations

import statsmodels.api as sm
from statsmodels.tsa.arima_process import arma_generate_sample

import itertools

# Select gpu
import os
gpu = sys.argv[-4]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

# Simulate time series using ARMA model 

analysis = sys.argv[-1] # 'treated' or 'control'
dataname = sys.argv[-2]
print('Generating {} data for analysis on {}'.format(dataname, analysis))

n_post  = 5
n_pre = 42-(n_post*2)
seq_len = 42

nb_features = 778

arparams = np.array([.75, -.25])
maparams = np.array([.65, .35])
arparams = np.r_[1, -arparams] # add zero-lag and negate
maparam = np.r_[1, maparams] # add zero-lag

x = arma_generate_sample(arparams, maparams, seq_len+n_post*2)

for _ in itertools.repeat(None, nb_features-1):
    x = np.c_[x, arma_generate_sample(arparams, maparams, seq_len+n_post*2)]

X_train = x[:seq_len]

X_test = x[seq_len:]

y = arma_generate_sample(arparams, maparams, seq_len+n_post*2)

y = y.reshape((y.shape[0], 1))

y_train = y[:seq_len]

y_test = y[seq_len:] - abs(y[seq_len:]*0.1) 

print('X_train shape:', X_train.shape)

print('X_test shape:', X_test.shape)

print('y_train shape:', y_train.shape)

print('y_test shape:', y_test.shape)

dX, dY = [], []
for i in range(seq_len-n_pre-n_post):
	dX.append(X_train[i:i+n_pre])
	dY.append(y_train[i+n_pre:i+n_pre+n_post])

dataX = np.array(dX)
dataY = np.array(dY)

print('dataX shape:', dataX.shape)
print('dataY shape:', dataY.shape)

## Save train and test sets to disk
print('Saving simulation data')

pkl.dump(X_train, open('data/votediff_x_train_sim.np', 'wb')) 
pkl.dump(X_test, open('data/votediff_x_test_sim.np', 'wb'))

pkl.dump(y_train, open('data/votediff_y_train_sim.np', 'wb'))
pkl.dump(y_test, open('data/votediff_y_test_sim.np', 'wb'))

np.savetxt('data/votediff_y_train_sim.csv', y_train, delimiter=",")
np.savetxt('data/votediff_y_test_sim.csv', y_test, delimiter=",")

# Define network structure

epochs = int(sys.argv[-3])

output_dim = 1

# Define model parameters

dropout = 0.8
hidden_dropout = 0.5
penalty = 5
batch_size = 2
activation = 'linear'
initialization = 'glorot_normal'

# Initiate sequential model

print('Initializing model')

inputs = Input(shape=(n_pre, nb_features,))

a = Permute((2, 1))(inputs)
a = Reshape((nb_features, n_pre))(a)
a = Dense(n_pre, activation='softmax')(a)
a_probs = Permute((2, 1), name='attention_vec')(a)
output_attention_mul = merge([inputs, a_probs], name='attention_mul', mode='mul')
dropout_1 = Dropout(dropout)(output_attention_mul)
lstm_1 = LSTM(1024, kernel_initializer=initialization, dropout=hidden_dropout, return_sequences=False)(dropout_1) # Encoder
repeat = RepeatVector(n_post)(lstm_1) # get the last output of the LSTM and repeats it 
lstm_2 = LSTM(640, kernel_initializer=initialization, return_sequences=True)(repeat)  # Decoder
output= TimeDistributed(Dense(1, activation=activation, kernel_regularizer=regularizers.l2(penalty)))(lstm_2)

model = Model(inputs=inputs, output=output)

model.compile(loss="mean_absolute_percentage_error", optimizer=Adam(lr=0.001, clipnorm=2))

print(model.summary())

#model.load_weights("results/elections/{}".format(dataname) + "/weights.2199-3.78.hdf5") # load weights

# Prepare model checkpoints and callbacks

filepath="results/elections_sim/{}".format(dataname) + "/weights.{epoch:02d}-{val_loss:.3f}.hdf5"
checkpointer = ModelCheckpoint(filepath=filepath, monitor='val_loss', verbose=1, period=1, save_best_only=True)

# Train model
print('Training')
csv_logger = CSVLogger('results/elections_sim/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=True)

model.fit(dataX, 
  dataY,
  batch_size=batch_size,
  verbose=1,
  shuffle=False, 
  epochs=epochs,
  callbacks=[checkpointer,csv_logger],
  validation_split= 0.1) 