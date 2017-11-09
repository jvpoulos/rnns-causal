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
from keras.models import Model
from keras.layers import LSTM, Dense, Permute, Reshape, Input, merge, Masking, RepeatVector, TimeDistributed, Dropout
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

y_test = pkl.load(open('data/{}_y_test_{}.np'.format(dataname,analysis), 'rb')) 

mask = -1 # mask value for inputs

# Define network structure

epochs = int(sys.argv[-3])
nb_features = X_train.shape[1]
output_dim = 1

n_pre = 47 # pre: 1948:2004 (NI)
n_post  = 5 # post: 2005:2010 (NI)

# Define model parameters

dropout = 0.2
hidden_dropout = 0
penalty = 0
batch_size = 1
activation = 'linear'
initialization = 'glorot_normal'

# Reshape X to three dimensions
# Should have shape (nb_samples, nb_timesteps, nb_features)

X_train = np.array(np.resize(X_train, (n_pre, n_pre, nb_features )))

print('X_train shape:', X_train.shape)

# Reshape y to three dimensions
# Should have shape (nb_samples, nb_timesteps, nb_features)

y_test = np.resize(y_test,  (n_pre, n_post, output_dim))

print('y_test shape:', y_test.shape)

# Initiate sequential model

print('Initializing model')

inputs = Input(shape=(n_pre, nb_features,))

a = Permute((2, 1))(inputs)
a = Reshape((nb_features, n_pre))(a)
a = Dense(n_pre, activation='softmax')(a)
a_probs = Permute((2, 1), name='attention_vec')(a)
output_attention_mul = merge([inputs, a_probs], name='attention_mul', mode='mul')
mask_layer = Masking(mask_value=mask)(output_attention_mul)
dropout_1 = Dropout(dropout)(mask_layer)
lstm_1 = LSTM(640, kernel_initializer=initialization, dropout=hidden_dropout, return_sequences=False)(dropout_1) # Encoder
repeat = RepeatVector(n_post)(lstm_1) # get the last output of the LSTM and repeats it
lstm_2 = LSTM(256, kernel_initializer=initialization, return_sequences=True)(repeat)  # Decoder
output= TimeDistributed(Dense(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty)))(lstm_2)

model = Model(inputs=inputs, output=output)

model.compile(loss="mean_absolute_percentage_error", optimizer=Adam(lr=0.002))

print(model.summary())

#model.load_weights("results/elections/{}".format(dataname) + "/weights.189-0.12.hdf5") # load weights

# Prepare model checkpoints and callbacks

filepath="results/elections/{}".format(dataname) + "/weights.{epoch:02d}-{val_loss:.2f}.hdf5"
checkpointer = ModelCheckpoint(filepath=filepath, monitor='val_loss', verbose=1, period=10, save_best_only=True)

# Train model
print('Training')
csv_logger = CSVLogger('results/elections/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=True)

model.fit(X_train,
  y_test,
  batch_size=batch_size,
  verbose=1,
  shuffle=False, 
  epochs=epochs,
  callbacks=[checkpointer,csv_logger],
  validation_split= 0.05) 