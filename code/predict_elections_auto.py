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

n_post  = 5
n_pre = 47-(n_post*2)
seq_len = 47

X_train = np.array(pkl.load(open('data/{}_x_train_{}.np'.format(dataname,analysis), 'rb')))[n_post:] # all but first n timesteps of training x

print('X_train shape:', X_train.shape)

X_test = np.array(pkl.load(open('data/{}_x_test_{}.np'.format(dataname,analysis), 'rb')))

print('X_test shape:', X_test.shape)

X = np.concatenate((X_train, X_test), axis=0)

print('X concatenated shape:', X.shape)
 
dX = []
for i in range(seq_len-n_pre-n_post):
	dX.append(X[i:i+n_pre])

dataX = np.array(dX)

print('dataX shape:', dataX.shape)

# Define network structure

nb_features = dataX.shape[2]
#output_dim = 24
output_dim = 5

# Define model parameters

dropout = 0.8
penalty = 0.01
batch_size = 2
initialization = 'glorot_normal'
activation = 'linear'

# Initiate sequential model

print('Initializing model')

inputs = Input(shape=(n_pre, nb_features,))

a = Permute((2, 1))(inputs)
a = Reshape((nb_features, n_pre))(a)
a = Dense(n_pre, activation='softmax')(a)
a_probs = Permute((2, 1), name='attention_vec')(a)
output_attention_mul = merge([inputs, a_probs], name='attention_mul', mode='mul')
dropout_1 = Dropout(dropout)(output_attention_mul)
lstm_1 = LSTM(1024, kernel_initializer=initialization, return_sequences=False)(dropout_1) # Encoder
repeat = RepeatVector(n_post)(lstm_1) # get the last output of the LSTM and repeats it 
lstm_2 = LSTM(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty), kernel_initializer=initialization, return_sequences=True)(repeat)  # Decoder

model = Model(inputs=inputs, output=lstm_2)

encoder = Model(inputs=inputs, output=lstm_1)

model.compile(loss="mean_absolute_percentage_error", optimizer=Adam(lr=0.001, clipnorm=5))

# Visualize model

# plot_model(model, to_file='results/elections_auto/{}/model.png'.format(dataname), # Plot graph of model
#   show_shapes = False,
#   show_layer_names = False)

#model_to_dot(model,show_shapes=True,show_layer_names = False).write('results/elections_auto/{}/model.dot'.format(dataname), format='raw', prog='dot') # write to dot file

# Load weights
filename = sys.argv[-3]
model.load_weights(filename, by_name=True)

print("Created model and loaded weights from file")

# Evaluation 

print('Generate predictions')

y_pred_test = model.predict(dataX, batch_size=batch_size, verbose=1) # generate test predictions

y_pred_test = np.mean(y_pred_test, axis=0).squeeze() # mean across # samples

print('predictions shape =', y_pred_test.shape)

np.savetxt("{}-{}-test.csv".format(filename,dataname), y_pred_test, delimiter=",")

# Get attention weights 

attention_vector = get_activations(model, dataX, print_shape_only=True, layer_name='attention_vec')[0]

attention_vector = np.mean(attention_vector, axis=0).squeeze() # mean across # samples

print('attention shape =', attention_vector.shape)

np.savetxt('results/elections_auto/{}/attention.csv'.format(dataname), attention_vector, delimiter=',') # save attentions to file