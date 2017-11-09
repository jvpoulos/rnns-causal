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

X_train = pkl.load(open('data/{}_x_train_{}.np'.format(dataname,analysis), 'rb')) 

X_test = pkl.load(open('data/{}_x_test_{}.np'.format(dataname,analysis), 'rb')) 

y_test = pkl.load(open('data/{}_y_test_{}.np'.format(dataname,analysis), 'rb')) 

mask = -1 # mask value

# Define network structure

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

X_test = np.array(np.resize(X_test, (n_pre, n_pre, nb_features )))

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

# Visualize model

plot_model(model, to_file='results/elections/{}/model.png'.format(dataname), # Plot graph of model
  show_shapes = False,
  show_layer_names = False)

#model_to_dot(model,show_shapes=True,show_layer_names = False).write('results/elections/{}/model.dot'.format(dataname), format='raw', prog='dot') # write to dot file

# Load weights
filename = sys.argv[-3]
model.load_weights(filename)

print("Created model and loaded weights from file")

# Evaluation 

print('Generate predictions')

y_pred_test = model.predict(X_train, batch_size=batch_size, verbose=1) # generate output predictions

np.savetxt("{}-{}-test.csv".format(filename,dataname), y_pred_test, delimiter=",")

# Get attention weights 
attention_vector = get_activations(model, X_train, print_shape_only=True, layer_name='attention_vec')[0]   

attention_vector = np.array(np.resize(attention_vector, (n_pre, nb_features )))

print('attention shape =', attention_vector.shape)

np.savetxt('results/elections/{}/attention.csv'.format(dataname), attention_vector, delimiter=',') # save attentions to file

# # plot

# pd.DataFrame(attention_vector, columns=['attention (%)']).plot(kind='bar',title='Attention as function of features')
# plt.savefig('results/land/{}/attention.png'.format(dataname))