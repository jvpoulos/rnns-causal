from __future__ import print_function

import sys
import math
import numpy as np
import pandas as pd

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Input, TimeDistributed, Dense, RepeatVector
from keras.callbacks import ModelCheckpoint, CSVLogger, EarlyStopping
from keras import regularizers
from keras.optimizers import Adam

from functools import partial, update_wrapper

def wrapped_partial(func, *args, **kwargs):
    partial_func = partial(func, *args, **kwargs)
    update_wrapper(partial_func, func)
    return partial_func

def weighted_rmse(y_true, y_pred, weights):
    return K.sqrt(K.mean(K.square(y_true - y_pred) * weights, axis=-1))

# Select gpu
import os
gpu = sys.argv[-7]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

imp = sys.argv[-1]
T = sys.argv[-2] 
t0 = sys.argv[-3] 
dataname = sys.argv[-4] 
nb_batches = int(sys.argv[-5])
nb_epochs = int(sys.argv[-6])

def create_model(n_pre, n_post, nb_features, output_dim):
    """ 
        creates, compiles and returns a RNN model 
        @param nb_features: the number of features in the model
    """
    # Define model parameters

    penalty=0.001
    lr = 0.0005

    encoder_hidden = 128
    decoder_hidden = 128

    inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    weights_tensor = Input(shape=(n_pre, nb_features), name="Weights")
    lstm_1 = LSTM(encoder_hidden, return_sequences=True, name='LSTM_1')(inputs) # Encoder
    lstm_2 = LSTM(encoder_hidden, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    lstm_3 = LSTM(decoder_hidden, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(lstm_3)

    cl = wrapped_partial(weighted_rmse, weights=weights_tensor)

    model = Model([inputs, weights_tensor], output)

    model.compile(optimizer=Adam(lr=lr), loss=cl)

    print(model.summary()) 

    return model

def train_model(model, dataX, dataY, weights, nb_epoches, nb_batches):

    # Prepare model checkpoints and callbacks

    filepath="../results/encoder-decoder/{}".format(dataname) + "/weights.{epoch:02d}-{val_loss:.3f}.hdf5"
    checkpointer = ModelCheckpoint(filepath=filepath, monitor='val_loss', verbose=1, period=5, save_best_only=True)

    stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=50, verbose=1, mode='auto')

    csv_logger = CSVLogger('../results/encoder-decoder/{}/training_log_{}_{}.csv'.format(dataname,dataname,imp), separator=',', append=False)

    history = model.fit(x=[dataX,weights], 
        y=dataY, 
        batch_size=nb_batches, 
        verbose=1,
        epochs=nb_epoches, 
        callbacks=[checkpointer,csv_logger,stopping],
        validation_split=0.2)

def test_model():

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    wx = np.array(pd.read_csv("../data/{}-wx-{}.csv".format(dataname,imp)))    

    print('raw wx shape', wx.shape)  

    wX = []
    for i in range(seq_len-n_pre-n_post):
        wX.append(wx[i:i+n_pre]) # controls are inputs
    
    wXC = np.array(wX)

    print('wXC shape:', wXC.shape)
    
    x = np.array(pd.read_csv("../data/{}-x-{}.csv".format(dataname,imp)))    

    print('raw x shape', x.shape)   

    dXC, dYC = [], []
    for i in range(seq_len-n_pre-n_post):
        dXC.append(x[i:i+n_pre]) # controls are inputs
        dYC.append(x[i+n_pre:i+n_pre+n_post]) # controls are outputs
        # dY.append(data[i+n_pre])
    
    dataXC = np.array(dXC)
    dataYC = np.array(dYC)

    print('dataXC shape:', dataXC.shape)
    print('dataYC shape:', dataYC.shape)

    nb_features = dataXC.shape[2]
    output_dim = dataYC.shape[2]

    # create and fit the encoder-decoder network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim)
    train_model(model, dataXC, dataYC, wXC, int(nb_epochs), int(nb_batches))

    # now test

    print('Generate predictions on full training set')

    preds_train = model.predict([dataXC,wXC], batch_size=int(nb_batches), verbose=1)

    preds_train = np.squeeze(preds_train)

    print('predictions shape =', preds_train.shape)

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-train-{}.csv'.format(dataname,dataname,imp))

    np.savetxt("../results/encoder-decoder/{}/encoder-decoder-{}-train-{}.csv".format(dataname,dataname,imp), preds_train, delimiter=",")

    print('Generate predictions on test set')

    wy = np.array(pd.read_csv("../data/{}-wy-{}.csv".format(dataname,imp)))    

    print('raw wy shape', wy.shape)  

    wY = []
    for i in range(seq_len-n_pre-n_post):
        wY.append(wy[i:i+n_pre]) # controls are inputs
    
    wXT = np.array(wY)

    print('wXT shape:', wXT.shape)

    y = np.array(pd.read_csv("../data/{}-y-{}.csv".format(dataname,imp)))
     
    print('raw y shape', y.shape)   

    dXT = []
    for i in range(seq_len-n_pre-n_post):
        dXT.append(y[i:i+n_pre]) # treated is input

    dataXT = np.array(dXT)

    print('dataXT shape:', dataXT.shape)

    preds_test = model.predict([dataXT, wXT], batch_size=int(nb_batches), verbose=1)

    preds_test = np.squeeze(preds_test)

    print('predictions shape =', preds_test.shape)

    print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test-{}.csv'.format(dataname,dataname,imp))

    np.savetxt("../results/encoder-decoder/{}/encoder-decoder-{}-test-{}.csv".format(dataname,dataname,imp), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()