from __future__ import print_function

import os.path
from os import path

import sys
import math
import numpy as np
import pandas as pd

import keras
import tensorflow as tf

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, GRU, TimeDistributed, Input, Dense, RepeatVector
from keras.callbacks import CSVLogger, EarlyStopping, TerminateOnNaN
from keras import regularizers
from keras.optimizers import Adam

from functools import partial, update_wrapper

def wrapped_partial(func, *args, **kwargs):
    partial_func = partial(func, *args, **kwargs)
    update_wrapper(partial_func, func)
    return partial_func

def weighted_mse(y_true, y_pred, weights):
    return K.mean(K.square(y_true - y_pred) * weights, axis=-1)

# Select gpu
import os
if gpu < 3:
    os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
    os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

    from tensorflow.python.client import device_lib
    print(device_lib.list_local_devices())

# Create directories
results_directory = 'results/encoder-decoder/{}'.format(dataname)

if not os.path.exists(results_directory):
    os.makedirs(results_directory)

def create_model(n_pre, n_post, nb_features, output_dim, lr, penalty, dr, encoder_hidden_1, encoder_hidden_2, decoder_hidden, activation):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    hidden_activation = activation

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    weights_tensor = Input(shape=(n_pre, nb_features), name="Weights")
    lstm_1 = LSTM(int(encoder_hidden_1), dropout=dr, recurrent_dropout=dr, activation=hidden_activation, return_sequences=True, name='LSTM_1')(inputs) # Encoder
    lstm_2 = LSTM(int(encoder_hidden_2), activation=hidden_activation, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    gru_1 = GRU(int(decoder_hidden), activation=hidden_activation, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, activation='linear', kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(gru_1)

    model = Model([inputs, weights_tensor], output)

    # Compile
    cl = wrapped_partial(weighted_mse, weights=weights_tensor)
    model.compile(optimizer=Adam(lr=lr), loss=cl)

    return model

def train_model(model, dataX, dataY, weights, epoch_count, batches):

    # Prepare model checkpoints and callbacks

    stopping = EarlyStopping(monitor='val_loss', patience=int(patience), min_delta=0, verbose=1, mode='min', restore_best_weights=True)

    terminate = TerminateOnNaN()

    # Model fit

    history = model.fit([dataX,weights], 
        dataY, 
        batch_size=batches, 
        verbose=1,
        epochs=epoch_count, 
        callbacks=[stopping,terminate],
        validation_split=0.2)

def test_model():

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    wx = np.array(pd.read_csv("data/{}-wx.csv".format(dataname)))

    print('raw wx shape', wx.shape)  

    wXC = []
    for i in range(seq_len-n_pre-n_post):
        wXC.append(wx[i:i+n_pre]) 

    wXC = np.array(wXC)

    print('wXC shape:', wXC.shape)

    x = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))

    print('raw x shape', x.shape) 
 
    dXC, dYC = [], []
    for i in range(seq_len-n_pre-n_post):
        dXC.append(x[i:i+n_pre])
        dYC.append(x[i+n_pre:i+n_pre+n_post])

    dataXC = np.array(dXC)
    dataYC = np.array(dYC)
    
    print('dataXC shape:', dataXC.shape)
    print('dataYC shape:', dataYC.shape)

    nb_features = dataXC.shape[2]
    output_dim = dataYC.shape[2]

    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim, lr, penalty, dr, encoder_hidden_1, encoder_hidden_2, decoder_hidden, activation)

    train_model(model, dataXC, dataYC, wXC, int(epochs), int(nb_batches))

    # now test

    print('Generate predictions on test set')

    wy = np.array(pd.read_csv("data/{}-wy.csv".format(dataname)))

    print('raw wy shape', wy.shape)  

    wY = []
    for i in range(seq_len-n_pre-n_post):
        wY.append(wy[i:i+n_pre]) # weights for outputs

    wXT = np.array(wY)

    print('wXT shape:', wXT.shape)

    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))

    print('raw y shape', y.shape)  

    dXT = []
    for i in range(seq_len-n_pre-n_post):
        dXT.append(y[i:i+n_pre]) # treated is input

    dataXT = np.array(dXT)

    print('dataXT shape:', dataXT.shape)

    preds_test = model.predict([dataXT, wXT], batch_size=int(nb_batches), verbose=1)

    print('predictions shape =', preds_test.shape)

    preds_test = np.squeeze(preds_test)

    print('predictions shape (squeezed)=', preds_test.shape)

    # Save predictions

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test.csv'.format(dataname,dataname))

    np.savetxt("results/encoder-decoder/{}/encoder-decoder-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()