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
from keras.layers import LSTM, Input, RepeatVector, Dropout, GRU
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

def create_model(n_pre, n_post, nb_features, output_dim):
    """ 
        creates, compiles and returns a RNN model 
        @param n_pre: the number of previous time steps (input)
        @param n_post: the number of posterior time steps (output or predictions)
        @param nb_features: the number of features in the model
        @param output_dim: the dimension of the target sequence
    """
    # Define model parameters
    
    dropout = 0.2 
    penalty = 0.0001
    initialization = 'glorot_normal'
    lr = 0.001 

    encoder_hidden = 256
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features,), name="Inputs")
    dropout_1 = Dropout(dropout, name="Dropout")(inputs)
    lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Encoder_1')(dropout_1) # Encoder
    lstm_2 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Encoder_2')(lstm_1) # Encoder
    lstm_3 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Encoder_3')(lstm_2) # Encoder
    lstm_4 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=False, name='Encoder_4')(lstm_3) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_4) # get the last output of the LSTM and repeats it
    gru_1 = GRU(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder_1')(repeat)  # Decoder
    gru_2 = GRU(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder_2')(gru_1)  # Decoder
    gru_3 = GRU(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder_3')(gru_2)  # Decoder
    gru_4 = GRU(nb_features, kernel_initializer=initialization, return_sequences=True, name='Decoder_4')(gru_3)  # Decoder

    model = Model(inputs=inputs, output=gru_4)

    model.compile(loss="mean_squared_error", optimizer=Adam(lr=lr, clipnorm=5))  

    return model

def test_sinus():
    ''' 
        testing how well the network can predict
        a simple sinus wave.
    '''
    # Load saved data

    # n_post  = 5 # elections/sim
    # n_pre =  15
    # seq_len = 47

    n_post  = 8 # Basque
    n_pre =  15
    seq_len = 35

    print('Load saved {} data for analysis on {}'.format(dataname, analysis))

    y_train = np.array(pkl.load(open('data/{}_y_train_{}.np'.format(dataname,analysis), 'rb')))[n_post:] # all but first n_post timesteps of training x

    print('y_train shape:', y_train.shape)

    y_test = np.array(pkl.load(open('data/{}_y_test_{}.np'.format(dataname,analysis), 'rb')))

    print('y_test shape:', y_test.shape)

    y = np.concatenate((y_train, y_test), axis=0)

    print('y concatenated shape:', y.shape)

    dX = []
    for i in range(seq_len-n_pre-n_post):
        dX.append(y[i:i+n_pre])

    dataX = np.array(dX)
  
    print('dataX shape:', dataX.shape)

    nb_features = dataX.shape[2]
    output_dim = 25 # basque:1 elections:25 sim:5

    # create and fit the LSTM network
    model = create_model(n_pre, n_post, nb_features, output_dim)
    # Load weights
    filename = sys.argv[-3]
    model.load_weights(filename, by_name=True)

    print("Created model and loaded weights from file")
    
    # now test

    print('Generate predictions')

    predict = model.predict(dataX, batch_size=BATCHES, verbose=1)[-1] # get last sample

    print('predictions shape =', predict.shape)

    np.savetxt("{}-{}-test.csv".format(filename,dataname), predict, delimiter=",")

   # # Visualize model

   #  plot_model(model, to_file='results/elections-auto/{}/autoencoder.png'.format(dataname), # Plot graph of model
   #  show_shapes = False,
   #  show_layer_names = True)

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
    #     plt.savefig('results/plots/elections_sim/plot_mtm_triple_' + str(i) + '.png')
    #     plt.cla()

def main():
    test_sinus()
    return 1

if __name__ == "__main__":
    sys.exit(main())