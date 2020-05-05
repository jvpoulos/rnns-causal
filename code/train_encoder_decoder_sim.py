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
from keras.layers import LSTM, Input, Masking, Dense, RepeatVector
from keras.callbacks import EarlyStopping, TerminateOnNaN
from keras import regularizers
from keras.optimizers import Adam
from keras_self_attention import SeqSelfAttention

from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()

# Select gpu
import os
if gpu < 3:
    os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
    os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

    from tensorflow.python.client import device_lib
    print(device_lib.list_local_devices())

# Create directories
results_directory = 'results/encoder-decoder/{}'.format(dataname)
data_directory = 'data/{}'.format(dataname)

if not os.path.exists(results_directory):
    os.makedirs(results_directory)

if not os.path.exists(data_directory):
    os.makedirs(data_directory)

def create_model(n_pre, n_post, nb_features, output_dim, lr, penalty, dr):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    encoder_hidden = 128
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    mask = Masking(mask_value=0.)(inputs)
    lstm_1 = LSTM(encoder_hidden, dropout=dr, return_sequences=True, name='LSTM_1')(mask) # Encoder
    lstm_2 = LSTM(encoder_hidden, dropout=dr, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    lstm_3 = LSTM(decoder_hidden, return_sequences=True, name='Decoder')(repeat)  # Decoder
    attn = SeqSelfAttention(attention_activation='sigmoid')(lstm_3)
    output= Dense(output_dim, kernel_regularizer=regularizers.l2(penalty), name='Dense')(attn)

    model = Model(inputs, output)

    # Compile
    model.compile(optimizer=Adam(lr=lr), loss='mse')

    return model

def train_model(model, dataX, dataY, epoch_count, batches):

    # Prepare model checkpoints and callbacks

    stopping = EarlyStopping(monitor='val_loss', patience=int(patience), min_delta=0.001, verbose=1, mode='min', restore_best_weights=True)

    terminate = TerminateOnNaN()

    # Model fit

    history = model.fit(dataX, 
        dataY, 
        batch_size=batches, 
        verbose=1,
        epochs=epoch_count, 
        callbacks=[stopping,terminate],
        validation_split=0.1)

def test_model():

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

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
    model = create_model(n_pre, n_post, nb_features, output_dim, lr, penalty, dr)

    # Load pre-trained weights
    weights_path = 'results/encoder-decoder/{}'.format(dataname) +'/weights-placebo-{}-{}.h5'.format(str(n_pre), str(nb_features))
    if path.exists(weights_path):
        print("loading weights from", weights_path)
        model.load_weights(weights_path)

    train_model(model, dataXC, dataYC, int(epochs), int(nb_batches))

    # save weights
    model.save_weights('results/encoder-decoder/{}'.format(dataname) +'/weights-placebo-{}-{}.h5'.format(str(n_pre),str(nb_features)))

    # now test

    print('Generate predictions on test set')

    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))

    y_scaled = scaler.transform(y)
     
    print('raw y shape', y_scaled.shape)  

    dXT = []
    for i in range(seq_len-n_pre):
        dXT.append(y_scaled[i:i+n_pre]) # treated is input

    dataXT = np.array(dXT)

    print('dataXT shape:', dataXT.shape)

    preds_test = model.predict(dataXT, batch_size=int(nb_batches), verbose=1)

    preds_test = np.squeeze(preds_test)

    print('predictions shape (squeezed)=', preds_test.shape)

    preds_test = scaler.inverse_transform(preds_test) # reverse scaled preds to actual values

    # Save predictions

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test.csv'.format(dataname,dataname))

    np.savetxt("results/encoder-decoder/{}/encoder-decoder-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()