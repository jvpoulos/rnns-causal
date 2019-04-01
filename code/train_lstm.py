from __future__ import print_function

import sys
import math
import numpy as np
import pandas as pd

from keras import backend as K
from keras.models import Model
from keras.layers import LSTM, Input, Dropout
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras import regularizers
from keras.optimizers import Adam, SGD

# Select gpu
import os
#gpu = sys.argv[-4]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

# analysis = sys.argv[-1] # 'treated' or 'control'
# dataname = sys.argv[-2] 

# EPOCHS = int(sys.argv[-3])

BATCHES = 4

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

    inputs = Input(shape=(n_pre, nb_features,), name="Inputs")  
    dropout_1 = Dropout(dr)(inputs)
    lstm_1 = LSTM(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty), kernel_initializer=initialization, return_sequences=False)(dropout_1) 

    model = Model(inputs=inputs, output=lstm_1)

    model.compile(loss="mean_squared_error", optimizer=Adam(lr=lr)) 

    print(model.summary()) 

    return model

def train_sinus(model, dataX, dataY, epoch_count, batches):
    """ 
        trains only the sinus model
    """
    # Prepare model checkpoints and callbacks

    filepath="results/lstm/{}/{}".format(dataname,analysis) + "/weights.{epoch:02d}-{val_loss:.3f}.hdf5"
    checkpointer = ModelCheckpoint(filepath=filepath, monitor='val_loss', verbose=1, period=10, save_best_only=True)

    csv_logger = CSVLogger('results/lstm/{}/{}/training_log_{}_{}.csv'.format(dataname,analysis,dataname,analysis), separator=',', append=False)

    history = model.fit(dataX, 
        dataY, 
        batch_size=batches, 
        verbose=1,
        epochs=epoch_count, 
        callbacks=[checkpointer,csv_logger],
        validation_split=0.1)

def test_sinus():
    ''' 
        testing how well the network can predict
        a simple sinus wave.
    '''
    # Load saved data

    # if dataname == 'basque':
    #     n_post  = 1 
    #     n_pre =  14-1 
    #     seq_len = 43
    
    # if dataname == 'california':
    #     n_post  = 1 
    #     n_pre =  19-1 
    #     seq_len = 31

    # if dataname == 'germany':
    #     n_post  = 1 
    #     n_pre =  30-1 
    #     seq_len = 44  

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))
    x = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))    

    if analysis == 'treated': 
        print('raw x shape', x.shape)   

        print('raw y shape', y.shape)   

        dX, dY = [], []
        for i in range(seq_len-n_pre-n_post):
            dX.append(x[i:i+n_pre]) # controls are inputs
            # dY.append(y[i+n_pre:i+n_pre+n_post]) # treated is output
            dY.append(y[i+n_pre])

    if analysis == 'control': 

        print('raw x shape', x.shape)   

        dX, dY = [], []
        for i in range(seq_len-n_pre-n_post):
            dX.append(x[i:i+n_pre]) # controls are inputs
            # dY.append(x[i+n_pre:i+n_pre+n_post]) # controls are outputs
            dY.append(x[i+n_pre])
    
    dataX = np.array(dX)
    dataY = np.array(dY)

    print('dataX shape:', dataX.shape)
    print('dataY shape:', dataY.shape)

    nb_features = dataX.shape[2]
    output_dim = dataY.shape[1]

    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim)
    train_sinus(model, dataX, dataY, int(EPOCHS), BATCHES)

def main():
    test_sinus()
    return 1

if __name__ == "__main__":
    sys.exit(main())