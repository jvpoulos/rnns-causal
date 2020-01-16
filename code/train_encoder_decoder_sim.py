from __future__ import print_function

import sys
import math
import numpy as np
import pandas as pd

import keras
import tensorflow as tf

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Input, Dense, RepeatVector, TimeDistributed
from keras.callbacks import CSVLogger, EarlyStopping
from keras import regularizers
from keras.optimizers import Adam

from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler(feature_range = (0, 1))

from functools import partial, update_wrapper

def wrapped_partial(func, *args, **kwargs):
    partial_func = partial(func, *args, **kwargs)
    update_wrapper(partial_func, func)
    return partial_func

def weighted_mse(y_true, y_pred, weights):
    return K.mean(K.square(y_true - y_pred) * (weights/(1-weights)), axis=-1)

# Select gpu
import os
if gpu < 3:
    os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
    os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

    from tensorflow.python.client import device_lib
    print(device_lib.list_local_devices())

def create_model(n_pre, n_post, nb_features, output_dim, lr, penalty, dr):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    encoder_hidden = 128
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    weights_tensor = Input(shape=(n_pre, nb_features), name="Weights")
    lstm_1 = LSTM(encoder_hidden, dropout=dr, return_sequences=True, name='LSTM_1')(inputs) # Encoder
    lstm_2 = LSTM(encoder_hidden, dropout=dr, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    lstm_3 = LSTM(decoder_hidden, dropout=dr, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(lstm_3)

    cl = wrapped_partial(weighted_mse, weights=weights_tensor)

    model = Model([inputs, weights_tensor], output)

    model.compile(optimizer=Adam(lr=lr), loss=cl)

    return model

def train_model(model, dataX, dataY, weights, epoch_count, batches):

    # Prepare model checkpoints and callbacks

    stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=int(patience), verbose=0, mode='auto')

    csv_logger = CSVLogger('results/encoder-decoder/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=False)

    history = model.fit([dataX,weights], 
        dataY, 
        batch_size=batches, 
        verbose=1,
        epochs=epoch_count, 
        callbacks=[stopping,csv_logger],
        validation_split=0.1)

def test_model():

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    wx = np.array(pd.read_csv("data/{}-wx.csv".format(dataname)))

    wX = []
    for i in range(seq_len-n_pre):
        wX.append(wx[i:i+n_pre]) # controls are inputs
    
    wXC = np.array(wX)

    print('wXC shape:', wXC.shape)

    x_obs = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))
    x_scaled = scaler.fit_transform(x_obs)

    print('raw x shape', x_scaled.shape)   

    dXC, dYC = [], []
    for i in range(seq_len-n_pre):
        dXC.append(x_scaled[i:i+n_pre]) # controls are inputs
        dYC.append(x_scaled[i+n_pre]) # controls are outputs
    
    dataXC = np.array(dXC)
    dataYC = np.array(dYC)

    print('dataXC shape:', dataXC.shape)
    print('dataYC shape:', dataYC.shape)

    dataYC = np.expand_dims(dataYC, axis=1)
    print('dataYC expanded shape:', dataYC.shape)

    nb_features = dataXC.shape[2]
    output_dim = dataYC.shape[2]

    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim, lr, penalty, dr)
    train_model(model, dataXC, dataYC, wXC, int(epochs), int(nb_batches))

    # now test

    print('Generate predictions on test set')

    wy = np.array(pd.read_csv("data/{}-wy.csv".format(dataname)))

    wY = []
    for i in range(seq_len-n_pre):
        wY.append(wy[i:i+n_pre]) # controls are inputs
    
    wXT = np.array(wY)

    print('wXT shape:', wXT.shape)
    
    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))
     
    y_scaled = scaler.fit_transform(y)
     
    print('raw y shape', y_scaled.shape)   

    dXT = []
    for i in range(seq_len-n_pre):
        dXT.append(y_scaled[i:i+n_pre]) # treated is input

    dataXT = np.array(dXT)

    print('dataXT shape:', dataXT.shape)

    preds_test = model.predict([dataXT, wXT], batch_size=int(nb_batches), verbose=1)
    
    print('predictions shape =', preds_test.shape)

    preds_test = np.mean(preds_test, axis=1)

    print('predictions shape (squeezed) =', preds_test.shape)

    preds_test = scaler.inverse_transform(preds_test) # reverse scaled preds to actual values

    print('predictions shape (transformed) =', preds_test.shape)

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test.csv'.format(dataname,dataname))

    np.savetxt("results/encoder-decoder/{}/encoder-decoder-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()