
## Load best checkpointed model and make predictions

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
from keras.utils.vis_utils import plot_model, model_to_dot
from keras.models import Model
from keras.layers import LSTM, Dense, Input, RepeatVector, TimeDistributed, Dropout, Bidirectional
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras import regularizers
from keras.optimizers import Adam

from read_activations import get_activations

def set_trace():
    from IPython.core.debugger import Pdb
    import sys
    Pdb(color_scheme='Linux').set_trace(sys._getframe().f_back)

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

n_post  = 5
n_pre = 15
seq_len = 47+1

X_train = np.array(pkl.load(open('data/{}_x_train_{}.np'.format(dataname,analysis), 'rb')))#[n_post:] # all but first n timesteps of training x

print('X_train shape:', X_train.shape)

# X_test = np.array(pkl.load(open('data/{}_x_test_{}.np'.format(dataname,analysis), 'rb')))

# X = np.concatenate((X_train, X_test), axis=0)
 
dX = []
for i in range(seq_len-n_pre-n_post):
	dX.append(X_train[i:i+n_pre])

dataX = np.array(dX)

print('dataX shape:', dataX.shape)

# Define network structure

nb_features = dataX.shape[2]
output_dim = 24
#output_dim = 5

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

# Visualize model

# plot_model(model, to_file='results/elections/{}/encoder-decoder.png'.format(dataname), # Plot graph of model
#   show_shapes = False,
#   show_layer_names = True)

# model_to_dot(model,show_shapes=False,show_layer_names = True).write('results/elections/{}/model.dot'.format(dataname), format='raw', prog='dot') # write to dot file

# Load weights
filename = sys.argv[-3]
model.load_weights(filename, by_name=True)

print("Created model and loaded weights from file")

# Evaluation 

print('Generate predictions')

y_pred_test = model.predict(dataX, batch_size=batch_size, verbose=1) # generate test predictions 

print('predictions shape =', y_pred_test.shape)

y_pred_test = y_pred_test[-1] # get last sample

np.savetxt("{}-{}-test.csv".format(filename,dataname), y_pred_test, delimiter=",")