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
from keras.layers import LSTM, Dense, Input, RepeatVector, TimeDistributed, Dropout, Bidirectional
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras import regularizers
from keras.optimizers import Adam

# Select gpu
import os
gpu = sys.argv[-4]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

# Load saved data

analysis = sys.argv[-1] # 'treated' or 'control'
dataname = sys.argv[-2] # 'votediff' or 'sim'
print('Load saved {} data for analysis on {}'.format(dataname, analysis))

n_post  = 5
n_pre = 15
seq_len = 47+1

X_train = np.array(pkl.load(open('data/{}_x_train_{}.np'.format(dataname,analysis), 'rb')))

print('X_train shape:', X_train.shape)

y_train = np.array(pkl.load(open('data/{}_y_train_{}.np'.format(dataname,analysis), 'rb')))

print('y_train shape:', y_train.shape)
 
dX, dY = [], []
for i in range(seq_len-n_pre-n_post):
	dX.append(X_train[i:i+n_pre])
	dY.append(y_train[i+n_pre:i+n_pre+n_post])

dataX = np.array(dX)
dataY = np.array(dY)

print('dataX shape:', dataX.shape)
print('dataY shape:', dataY.shape)

# Define network structure

epochs = int(sys.argv[-3])
nb_features = dataX.shape[2]
output_dim = dataY.shape[2]

# Define model parameters

dropout = 0.8 
penalty = 0.5  
batch_size = 8
activation = 'linear'
initialization = 'glorot_normal'

encoder_hidden = 16 
decoder_hidden = 8 

# Initiate sequential model 

print('Initializing model')

inputs = Input(shape=(n_pre, nb_features,), name="Inputs")
dropout_1 = Dropout(dropout, name="Dropout")(inputs)
lstm_1 = Bidirectional(LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=False, name='LSTM'), name='Encoder')(dropout_1) # Encoder
repeat = RepeatVector(n_post, name='Repeat')(lstm_1) # get the last output of the LSTM and repeats it
lstm_2 = LSTM(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder')(repeat)  # Decoder
output= TimeDistributed(Dense(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty)), name='Outputs')(lstm_2)

model = Model(inputs=inputs, output=output)

model.compile(loss="mean_absolute_percentage_error", optimizer=Adam(lr=0.001, clipnorm=5))  

print(model.summary())

#model.load_weights("results/elections/{}".format(dataname) + "/weights.2199-3.78.hdf5") # load weights

# Prepare model checkpoints and callbacks

filepath="results/elections/{}".format(dataname) + "/weights.{epoch:02d}-{val_loss:.3f}.hdf5"
checkpointer = ModelCheckpoint(filepath=filepath, monitor='val_loss', verbose=1, period=100, save_best_only=True) 

# Train model
print('Training')
csv_logger = CSVLogger('results/elections/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=True)

model.fit(dataX, 
  dataY,
  batch_size=batch_size,
  verbose=1,
  shuffle=True, 
  epochs=epochs,
  callbacks=[checkpointer,csv_logger],
  validation_split= 0.01) 