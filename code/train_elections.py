# Softmax mask inside the network
# Gives normalized distribution of the importance of each time step (or unit) regarding an input.
# https://github.com/philipperemy/keras-attention-mechanism

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
from keras.models import Sequential, Model
from keras.layers import LSTM, Dense, Masking, Dropout, Activation, Permute, Reshape, Input, Flatten, merge
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras import regularizers
from keras.optimizers import Adam

from attention_utils import get_activations

# Select gpu
import os
gpu = sys.argv[-4]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

# Load saved data

analysis = sys.argv[-1] # 'treated' or 'control'
dataname = sys.argv[-2]
print('Load saved {} data for analysis on {}'.format(dataname, analysis))

X_train = pkl.load(open('data/{}_x_train_{}.np'.format(dataname,analysis), 'rb')) 
X_val = pkl.load(open('data/{}_x_val_{}.np'.format(dataname,analysis), 'rb'))

y_train = pkl.load(open('data/{}_y_train_{}.np'.format(dataname,analysis), 'rb')) 
y_val = pkl.load(open('data/{}_y_val_{}.np'.format(dataname,analysis), 'rb')) 

# Define network structure

epochs = int(sys.argv[-3])
nb_timesteps = 1
nb_features = X_train.shape[1]
output_dim = 1

# Define model parameters

dropout = 0.95
penalty = 0.05
batch_size = 42
nb_hidden = 256
activation = 'linear'
initialization = 'glorot_normal'

# # Reshape X to three dimensions
# # Should have shape (batch_size, nb_timesteps, nb_features)

X_train = np.resize(X_train, (X_train.shape[0], nb_timesteps, X_train.shape[1]))

print('X_train shape:', X_train.shape)

X_val = np.resize(X_val, (X_val.shape[0], nb_timesteps, X_val.shape[1]))

print('X_val shape:', X_val.shape)

# Reshape y to two dimensions
# Should have shape (batch_size, output_dim)

y_train = np.resize(y_train, (y_train.shape[0], output_dim))

print('y_train shape:', y_train.shape)

y_val = np.resize(y_val, (y_val.shape[0], output_dim))

print('y_val shape:', y_val.shape)

# Initiate sequential model

print('Initializing model')

inputs = Input(shape=(nb_timesteps, nb_features,))
a = Permute((2, 1))(inputs)
a = Reshape((nb_features, nb_timesteps))(a)
a = Dense(nb_timesteps, activation='sigmoid')(a)
a_probs = Permute((2, 1), name='attention_vec')(a)
output_attention_mul = merge([inputs, a_probs], name='attention_mul', mode='mul')
dropout_0 = Dropout(dropout)(output_attention_mul)
lstm_1 = LSTM(nb_hidden, kernel_initializer=initialization, return_sequences=True)(dropout_0) 
dropout_1 = Dropout(dropout)(lstm_1)
lstm_2 = LSTM(nb_hidden, kernel_initializer=initialization, return_sequences=True)(dropout_1) 
dropout_2 = Dropout(dropout)(lstm_2)
lstm_3 = LSTM(nb_hidden, kernel_initializer=initialization, return_sequences=False)(dropout_2)
output = Dense(output_dim, 
      activation=activation,
      kernel_regularizer=regularizers.l2(penalty))(lstm_3)
model = Model(input=[inputs], output=output)

print(model.summary())

#model.load_weights("results/elections/{}".format(dataname) + "/weights-0.14.hdf5", by_name=True) # load weights

# Configure learning process

model.compile(optimizer=Adam(lr=0.0005), 
              loss='mean_absolute_percentage_error',
              metrics=['mean_absolute_percentage_error'])

# Prepare model checkpoints and callbacks

filepath="results/elections/{}".format(dataname) + "/weights-{val_mean_absolute_percentage_error:.1f}.hdf5"
checkpointer = ModelCheckpoint(filepath=filepath, verbose=0, save_best_only=False)

# Train model
print('Training')
csv_logger = CSVLogger('results/elections/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=True)

model.fit(X_train,
  y_train,
  batch_size=batch_size,
  verbose=1,
  epochs=epochs,
  shuffle=True, 
  callbacks=[checkpointer,csv_logger],
  validation_data=(X_val, y_val))