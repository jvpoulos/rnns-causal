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
from keras.models import Model
from keras.layers import LSTM, Dense, Input, RepeatVector, TimeDistributed, Dropout
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
dataname = sys.argv[-2] # 'votediff' or 'sim'

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

    dropout = 0.5
    penalty = 0.01 
    activation = 'linear'
    initialization = 'glorot_normal'

    encoder_hidden = 256
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features,), name="Inputs")
    dropout_1 = Dropout(dropout, name="Dropout")(inputs)
    lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=False, name='Encoder')(dropout_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_1) # get the last output of the LSTM and repeats it
    lstm_2 = LSTM(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty)), name='Outputs')(lstm_2)

    model = Model(inputs=inputs, output=output)

    model.compile(loss="mean_squared_error", optimizer=Adam(lr=0.001, clipnorm=5))  

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

    y_train = np.array(pkl.load(open('data/{}_y_train_{}.np'.format(dataname,analysis), 'rb')))

    print('y_train shape:', y_train.shape)

    X_test = np.array(pkl.load(open('data/{}_x_test_{}.np'.format(dataname,analysis), 'rb')))

    print('X_test shape:', X_test.shape)

    X = np.concatenate((X_train, X_test, X_test), axis=0) # repeat test

    print('X concatenated shape:', X.shape)

    y_test = np.array(pkl.load(open('data/{}_y_test_{}.np'.format(dataname,analysis), 'rb')))

    print('y_test shape:', y_test.shape)

    y = np.concatenate((y_train, y_test, y_test), axis=0) # repeat test

    print('y concatenated shape:', y.shape)

    n_post  = 8
    n_pre =  35
    seq_len = n_pre + n_post*2

    dX, dY = [], []
    for i in range(seq_len-n_pre-n_post):
        dX.append(X[i:i+n_pre])
        dY.append(y[i+n_pre:i+n_pre+n_post])
        #dY.append(sinus[i+n_pre])
    dataX = np.array(dX)
    dataY = np.array(dY)

    print('dataX shape:', dataX.shape)
    print('dataY shape:', dataY.shape)

    nb_features = dataX.shape[2]
    output_dim = dataY.shape[2]

    # create and fit the LSTM network
    model = create_model(n_pre, n_post, nb_features, output_dim)
    
    # Load weights
    filename = sys.argv[-3]
    model.load_weights(filename, by_name=True)

    print("Created model and loaded weights from file")
    
    # now test

    print('Generate predictions')

    predict = model.predict(dataX, batch_size=BATCHES, verbose=1)

    print('predictions shape =', predict.shape)

    np.savetxt("{}-{}-test.csv".format(filename,dataname), predict, delimiter=",")

    # # now plot
    # nan_array = np.empty((n_pre - 1))
    # nan_array.fill(np.nan)
    # nan_array2 = np.empty(n_post)
    # nan_array2.fill(np.nan)
    # ind = np.arange(n_pre + n_post)

    # fig, ax = plt.subplots()
    # for i in range(0, n_pre, n_pre):

    #     forecasts = np.concatenate((nan_array, dataX[i, -1:, 0], predict[i, :, 0]))
    #     ground_truth = np.concatenate((nan_array, dataX[i, -1:, 0], dataY[i, :, 0]))
    #     network_input = np.concatenate((dataX[i, :, 0], nan_array2))
     
    #     ax.plot(ind, network_input, 'b-x', label='Network input')
    #     ax.plot(ind, forecasts, 'r-x', label='Many to many model forecast')
    #     ax.plot(ind, ground_truth, 'g-x', label = 'Ground truth')
        
    #     plt.xlabel('t')
    #     plt.ylabel('sin(t)')
    #     plt.title('Sinus Many to Many Forecast')
    #     plt.legend(loc='best')
    #     plt.savefig('results/basque/' + str(i) + '.png')
    #     plt.cla()

def main():
    test_sinus()
    return 1

if __name__ == "__main__":
    sys.exit(main())