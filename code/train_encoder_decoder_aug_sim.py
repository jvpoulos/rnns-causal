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
from keras.layers import LSTM, Input, GRU, TimeDistributed, Dense, RepeatVector, GaussianNoise, GaussianDropout, Concatenate
from keras.callbacks import CSVLogger
from keras import regularizers
from keras.optimizers import Adam

# Select gpu
import os
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())


def create_model(n_pre, n_post, nb_features, output_dim, dropout, GS, GD, multiple):
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

    if multiple>0:
    	input1 = Input(shape=(n_pre, nb_features), name="Input1")
    	input2 = Input(shape=(n_pre, nb_features), name="Input2")
    	inputs = Concatenate()([input1, input2], name="Inputs")
    else:
    	inputs = Input(shape=(n_pre, nb_features), name="Inputs")
    if GS>0:
    	noise_layer= GaussianNoise(GS)(inputs)
    	lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=True, name='LSTM_1')(noise_layer) # Encoder
    else:	
    	lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=True, name='LSTM_1')(inputs) # Encoder
    if GD>0:
    	noise_layer= GaussianDropout(GD)(inputs)
    	lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=True, name='LSTM_1')(noise_layer) # Encoder
    else:	
    	lstm_1 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=True, name='LSTM_1')(inputs) # Encoder
    lstm_2 = LSTM(encoder_hidden, kernel_initializer=initialization, dropout=dr, return_sequences=False, name='LSTM_2')(lstm_1) # Encoder
    repeat = RepeatVector(n_post, name='Repeat')(lstm_2) # get the last output of the LSTM and repeats it
    gru_1 = GRU(decoder_hidden, kernel_initializer=initialization, return_sequences=True, name='Decoder')(repeat)  # Decoder
    output= TimeDistributed(Dense(output_dim, activation=activation, kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(gru_1)

    if multiple>0:
    	model = Model(inputs=[input1,input2], output=output)
    else:
    	model = Model(inputs=inputs, output=output)
    model.compile(optimizer=Adam(lr=lr), loss="mean_squared_error")  

    return model

def train_model(model, dataX, dataY, epoch_count, batches):

    # Prepare model checkpoints and callbacks

    csv_logger = CSVLogger('../results/encoder-decoder/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=False)
    if multiple>0:
    	history = model.fit([dataX1,dataX2], 
        dataY, 
        batch_size=batches, 
        verbose=0,
        epochs=epoch_count, 
        callbacks=[csv_logger],
        validation_split=0.2)

    else:
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

    if multiple>0:
    	x2 = np.array(pd.read_csv("../data/{}-x2.csv".format(dataname)))
    	print('raw x2 shape', x2.shape)   

    	dXC2 = []
    	for i in range(seq_len-n_pre-n_post):
        	dXC2.append(x2[i:i+n_pre]) # controls are inputs
    
    	dataXC2 = np.array(dXC2)

    	print('dataXC2 shape:', dataXC2.shape)

    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, n_post, nb_features, output_dim, dropout, GS, GD, multiple)

    if multiple>0:
    	train_model(model, [dataXC,dataXC2], dataYC, int(epochs), int(nb_batches))
    else:
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

    if multiple>0:
    	print('Generate predictions on test set')

    	y2 = np.array(pd.read_csv("../data/{}-y2.csv".format(dataname)))

    	print('raw y2 shape', y2.shape)   

    	dXT2 = []
    	for i in range(seq_len-n_pre-n_post):
        	dXT2.append(y2[i:i+n_pre]) # treated is input

        dataXT2 = np.array(dXT2)

        print('dataXT2 shape:', dataXT2.shape)

        preds_test = model.predict([dataXT,dataXT2], batch_size=int(nb_batches), verbose=1)

        preds_test = np.squeeze(preds_test)

        print('predictions shape =', preds_test.shape)

        print('Saving to results/encoder-decoder/{}/encoder-decoder-{}-test.csv'.format(dataname,dataname))

        np.savetxt("../results/encoder-decoder/{}/encoder-decoder-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")

def main():
    test_model()
    return 1

if __name__ == "__main__":
    main()