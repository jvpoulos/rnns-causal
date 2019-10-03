from __future__ import print_function

import sys
import math
import numpy as np
import pandas as pd

import keras
import tensorflow as tf
gpu_options = tf.GPUOptions(allow_growth=True)
sess = tf.Session(config=tf.ConfigProto(gpu_options=gpu_options))
keras.backend.tensorflow_backend.set_session(sess)

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Input, GRU, TimeDistributed, Dense, RepeatVector
from keras.callbacks import CSVLogger
from keras import regularizers
from keras.optimizers import Adam

# Select gpu
import os
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())


def create_model(n_pre, n_post, nb_features, output_dim, dropout):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    initialization = 'glorot_normal'
    activation = 'linear'
    lr = 0.001
    penalty=0
    dr=dropout

    encoder_hidden = 128
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=True, name='LSTM_1')(inputs) # Encoder
    lstm_2 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    gru_1 = GRU(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(gru_1)

    model = Model(inputs=inputs, output=output)

    model.compile(optimizer=Adam(lr=lr), loss="mean_squared_error")  

    print(model.summary()) 

    return model

def train_model(model, dataX, dataY, epoch_count, batches):

    # Prepare model checkpoints and callbacks

    csv_logger = CSVLogger('../results/encoder-decoder/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=False)

    history = model.fit(dataX, 
        dataY, 
        batch_size=batches, 
        verbose=0,
        epochs=epoch_count, 
        callbacks=[csv_logger],
        validation_split=0.2)

def test_model():

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    x = np.array(pd.read_csv("../data/{}-x.csv".format(dataname)))    

    print('raw x shape', x.shape)   

    dXC, dYC = [], []
    for i in range(seq_len-n_pre-n_post):
        dXC.append(x[i:i+n_pre]) # controls are inputs
        dYC.append(x[i+n_pre:i+n_pre+n_post]) # controls are outputs
    
    dataXC = np.array(dXC)
    dataYC = np.array(dYC)

    print('dataXC shape:', dataXC.shape)
    print('dataYC shape:', dataYC.shape)

    nb_features = dataXC.shape[2]
    output_dim = dataYC.shape[2]

    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim, dropout)

    train_model(model, dataXC, dataYC, int(epochs), int(nb_batches))

    # now test

    print('Generate predictions on test set')

    y = np.array(pd.read_csv("../data/{}-y.csv".format(dataname)))
     
    print('raw y shape', y.shape)   

    dXT = []
    for i in range(seq_len-n_pre-n_post):
        dXT.append(y[i:i+n_pre]) # treated is input

    dataXT = np.array(dXT)

    print('dataXT shape:', dataXT.shape)

    preds_test = model.predict(dataXT, batch_size=int(nb_batches), verbose=1)

    preds_test = np.squeeze(preds_test)

    print('predictions shape =', preds_test.shape)

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test.csv'.format(dataname,dataname))

    np.savetxt("../results/encoder-decoder/{}/encoder-decoder-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()