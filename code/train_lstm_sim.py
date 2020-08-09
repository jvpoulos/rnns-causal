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
from keras.layers import LSTM, Input, Masking, Dense, Flatten
from keras.callbacks import EarlyStopping, TerminateOnNaN
from keras import regularizers
from keras.optimizers import Adam

from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()

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

# Create directories
results_directory = 'results/lstm/{}'.format(dataname)
data_directory = 'data/{}'.format(dataname)

if not os.path.exists(results_directory):
    os.makedirs(results_directory)

if not os.path.exists(data_directory):
    os.makedirs(data_directory)

def create_model(n_pre, nb_features, output_dim, lr, penalty, dr):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    n_hidden = 128

    hidden_activation = 'relu'

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    mask = Masking(mask_value=0.)(inputs)
    weights_tensor = Input(shape=(nb_features,), name="Weights")
    lstm_1 = LSTM(n_hidden, dropout=dr, activation=hidden_activation, return_sequences=False, name="LSTM_1")(mask) 
    output= Dense(output_dim, kernel_regularizer=regularizers.l2(penalty), name='Dense')(lstm_1)

    model = Model([inputs,weights_tensor], output) 

    # Compile
    cl = wrapped_partial(weighted_mse, weights=weights_tensor)
    model.compile(optimizer=Adam(lr=lr), loss=cl)

    return model

def train_model(model, dataX, dataY, weights, epoch_count, batches):

    # Prepare model checkpoints and callbacks

    stopping = EarlyStopping(monitor='val_loss', patience=int(patience), min_delta=0.001, verbose=1, mode='min', restore_best_weights=True)

    terminate = TerminateOnNaN()

    # Model fit

    history = model.fit(x=[dataX,weights], 
        y=dataY, 
        batch_size=batches, 
        verbose=1,
        epochs=epoch_count, 
        callbacks=[stopping,terminate],
        validation_split=0.1)

def test_model():

    n_pre = int(t0)-1
    seq_len = int(T)

    wx = np.array(pd.read_csv("data/{}-wx.csv".format(dataname)))  

    print('raw wx shape', wx.shape)  

    wXC = []
    for i in range(seq_len-n_pre):
        wXC.append(wx[i+n_pre]) # weights for outputs

    wXC = np.array(wXC)

    print('wXC shape:', wXC.shape)

    x = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))

    x_scaled = scaler.fit_transform(x)

    print('raw x shape', x_scaled.shape)   

    dXC, dYC = [], []
    for i in range(seq_len-n_pre):
        dXC.append(x_scaled[i:i+n_pre])
        dYC.append(x_scaled[i+n_pre])
    
    dataXC = np.array(dXC)
    dataYC = np.array(dYC)

    print('dataXC shape:', dataXC.shape)
    print('dataYC shape:', dataYC.shape)

    nb_features = dataXC.shape[2]
    output_dim = dataYC.shape[1]
  
    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, nb_features, output_dim, lr, penalty, dr)

    # load pre-trained weights
    weights_path = 'results/lstm/{}'.format(dataname) +'/weights-placebo-{}-{}.h5'.format(str(n_pre), str(nb_features))
    if path.exists(weights_path):
        print("loading weights from", weights_path)
        model.load_weights(weights_path)

    train_model(model, dataXC, dataYC, wXC, int(epochs), int(nb_batches))

    # save weights
    model.save_weights('results/lstm/{}'.format(dataname) +'/weights-placebo-{}-{}.h5'.format(str(n_pre),str(nb_features)))

    # now test

    print('Generate predictions on test set')

    wy = np.array(pd.read_csv("data/{}-wy.csv".format(dataname)))

    print('raw wy shape', wy.shape)  

    wY = []
    for i in range(seq_len-n_pre):
        wY.append(wy[i+n_pre]) # weights for outputs

    wXT = np.array(wY)

    print('wXT shape:', wXT.shape)

    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))
     
    y_scaled = scaler.transform(y)
     
    print('raw y shape', y_scaled.shape)  

    dXT = []
    for i in range(seq_len-n_pre):
        dXT.append(y_scaled[i:i+n_pre]) # treated is input

    dataXT = np.array(dXT)

    print('dataXT shape:', dataXT.shape)

    preds_test = model.predict([dataXT, wXT], batch_size=int(nb_batches), verbose=0)
    
    preds_test = scaler.inverse_transform(preds_test) # reverse scaled preds to actual values

    print('predictions shape =', preds_test.shape)

    # Save predictions

    print('Saving to results/lstm/{}/lstm-{}-test.csv'.format(dataname,dataname))

    np.savetxt("results/lstm/{}/lstm-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
	main()