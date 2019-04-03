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

import train_encoder_decoder

analysis = sys.argv[-1] # 'treated' or 'control'
T = sys.argv[-2] 
t0 = sys.argv[-3] 
dataname = sys.argv[-4] 
nb_batches = int(sys.argv[-5])

def test_model():

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
    model = train_encoder_decoder.create_model(n_pre, n_post, nb_features, output_dim)

    # Load weights
    filename = sys.argv[-7]
    model.load_weights(filename, by_name=True)

    print("Created model and loaded weights from file")
    
    # now test

    print('Generate predictions')

    predict = model.predict(dataX, batch_size=int(nb_batches), verbose=1)

    predict = np.squeeze(predict)

    print('predictions shape =', predict.shape)

    print('Saving to results/encoder-decoder/{}/{}/encoder-decoder-{}-{}-test.csv'.format(dataname,analysis,analysis,dataname))

    np.savetxt("../results/encoder-decoder/{}/{}/encoder-decoder-{}-{}-test.csv".format(dataname,analysis,analysis,dataname), predict, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    sys.exit(main())