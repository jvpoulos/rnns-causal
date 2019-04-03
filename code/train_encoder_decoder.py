from __future__ import print_function

import sys
import math
import numpy as np
import pandas as pd

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Input, GRU, TimeDistributed, Dense, RepeatVector, Dropout
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras import regularizers
from keras.optimizers import Adam

# Select gpu
import os
gpu = sys.argv[-6]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

T = sys.argv[-1] 
t0 = sys.argv[-2] 
dataname = sys.argv[-3] 
nb_batches = int(sys.argv[-4])
nb_epochs = int(sys.argv[-5])

def create_model(n_pre, n_post, nb_features, output_dim):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    initialization = 'glorot_normal'
    activation = 'linear'
    lr = 0.001
    penalty=0.1
    dr=0.5

    encoder_hidden = 128
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    dropout_1 = Dropout(dr)(inputs)
    lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=True, name='LSTM_1')(dropout_1) # Encoder
    lstm_2 = LSTM(encoder_hidden, kernel_initializer=initialization, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    gru_1 = GRU(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(gru_1)

    model = Model(inputs=inputs, output=output)

    model.compile(loss="mean_squared_error", optimizer=Adam(lr=lr))  

    print(model.summary()) 

    return model

def train_model(model, dataX, dataY, nb_epoches, nb_batches):

    # Prepare model checkpoints and callbacks

    filepath="../results/encoder-decoder/{}".format(dataname) + "/weights.{epoch:02d}-{val_loss:.3f}.hdf5"
    checkpointer = ModelCheckpoint(filepath=filepath, monitor='val_loss', verbose=1, period=5, save_best_only=True)

    csv_logger = CSVLogger('../results/encoder-decoder/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=False)

    history = model.fit(dataX, 
        dataY, 
        batch_size=nb_batches, 
        verbose=1,
        epochs=nb_epoches, 
        callbacks=[checkpointer,csv_logger],
        validation_split=0.2)

def test_model():

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    x = np.array(pd.read_csv("../data/{}-x.csv".format(dataname)))    

    print('raw x shape', x.shape)   

    dX, dY = [], []
    for i in range(seq_len-n_pre-n_post):
        dX.append(x[i:i+n_pre]) # controls are inputs
        dY.append(x[i+n_pre:i+n_pre+n_post]) # controls are outputs
        # dY.append(data[i+n_pre])
    
    dataX = np.array(dX)
    dataY = np.array(dY)

    print('dataX shape:', dataX.shape)
    print('dataY shape:', dataY.shape)

    nb_features = dataX.shape[2]
    output_dim = dataY.shape[2]

    # create and fit the encoder-decoder network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim)
    train_model(model, dataX, dataY, int(nb_epochs), int(nb_batches))

    # now test

    print('Generate predictions on full training set')

    predict = model.predict(dataX, batch_size=int(nb_batches), verbose=1)

    predict = np.squeeze(predict)

    print('predictions shape =', predict.shape)

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-train.csv'.format(dataname,dataname))

    np.savetxt("../results/encoder-decoder/{}/encoder-decoder-{}-train.csv".format(dataname,dataname), predict, delimiter=",")

    print('Generate predictions on test set')

    y = np.array(pd.read_csv("../data/{}-y.csv".format(dataname)))
     
    print('raw y shape', y.shape)   

    dX, dY = [], []
    for i in range(seq_len-n_pre-n_post):
        dX.append(y[i:i+n_pre]) # treated is input
        dY.append(y[i+n_pre:i+n_pre+n_post]) # treated is output
        # dY.append(data[i+n_pre])

    predict = model.predict(dataY, batch_size=int(nb_batches), verbose=1)

    predict = np.squeeze(predict)

    print('predictions shape =', predict.shape)

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test.csv'.format(dataname,dataname))

    np.savetxt("../results/encoder-decoder/{}/encoder-decoder-{}-test.csv".format(dataname,dataname), predict, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()