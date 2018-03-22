from __future__ import print_function

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import sys
import math
import numpy as np
import cPickle as pkl
import pandas as pd
from attrdict import AttrDict

from keras import backend as K
from keras.utils.vis_utils import plot_model, model_to_dot
from keras.models import Model
from keras.layers import LSTM, Dense, Input, RepeatVector, TimeDistributed, Dropout, Bidirectional, GRU
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

analysis = sys.argv[-1] # 'treated' or 'control'
dataname = sys.argv[-2] # 'votediff' 'sim'

BATCHES = 8

if dataname == 'california':
    BATCHES = 3

if dataname == 'germany':
    BATCHES = 3

def create_model(n_pre, n_post, nb_features, output_dim):
    """ 
        creates, compiles and returns a RNN model 
        @param n_pre: the number of previous time steps (input)
        @param n_post: the number of posterior time steps (output or predictions)
        @param nb_features: the number of features in the model
        @param output_dim: the dimension of the target sequence
    """
    # Define model parameters
    
    if dataname == 'basque':
        penalty = 0.01

    if dataname == 'germany':
        penalty = 0.001       

    if dataname == 'california':
        penalty = 0.001

    activation = 'linear'
    initialization = 'glorot_normal'
    lr = 0.001 

    inputs = Input(shape=(n_pre, nb_features,), name="Inputs")
    lstm_1 = LSTM(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty), kernel_initializer=initialization, return_sequences=False)(inputs) 

    model = Model(inputs=inputs, output=lstm_1)

    model.compile(loss="mean_squared_error", optimizer=Adam(lr=lr))  

    print(model.summary()) 

    return model

def test_sinus():
    ''' 
        testing how well the network can predict
        a simple sinus wave.
    '''
    # Load saved data

    print('Load saved {} data for analysis on {}'.format(dataname, analysis))

    X_train = np.array(pkl.load(open('data/{}_x_train_{}.np'.format(dataname,analysis), 'rb')))

    print('X_train shape:', X_train.shape)

    X_test = np.array(pkl.load(open('data/{}_x_test_{}.np'.format(dataname,analysis), 'rb')))

    print('X_test shape:', X_test.shape)

    X = np.concatenate((X_train, X_test), axis=0)

    print('X concatenated shape:', X.shape)

    if dataname == 'elections':
        n_post  = 5 
        n_pre =  15
        seq_len = 47

    if dataname == 'sim':
        n_post  = 5 
        n_pre =  15
        seq_len = 47

    if dataname == 'basque':
        n_post  = 1 
        n_pre =  14-1 
        seq_len = 43
    
    if dataname == 'california':
        n_post  = 1 
        n_pre =  19-1 
        seq_len = 31

    if dataname == 'germany':
        n_post  = 1 
        n_pre =  30-1 
        seq_len = 44   

    dX = []
    for i in range(seq_len-n_pre-n_post):
        dX.append(X[i:i+n_pre])

    dataX = np.array(dX)
  
    print('dataX shape:', dataX.shape)

    nb_features = dataX.shape[2]

    if dataname == 'california':
        output_dim = 1
    if dataname == 'germany':
        output_dim = 1
    if dataname == 'basque':
        output_dim = 1
    if dataname == 'elections':
        output_dim = 25
    if dataname == 'sim':
        output_dim = 5
        
    # create and fit the LSTM network
    model = create_model(n_pre, n_post, nb_features, output_dim)
    # Load weights
    filename = sys.argv[-3]
    model.load_weights(filename, by_name=True)

    print("Created model and loaded weights from file")
    
    # now test

    print('Generate predictions')

    predict = model.predict(dataX, batch_size=BATCHES, verbose=1)#[-1] # get last sample

    print('predictions shape =', predict.shape)

    np.savetxt("{}-{}-test.csv".format(filename,dataname), np.squeeze(predict), delimiter=",")


def main():
    test_sinus()
    return 1

if __name__ == "__main__":
    sys.exit(main())