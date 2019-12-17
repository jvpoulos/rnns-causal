from __future__ import print_function

import numpy as np
import pandas as pd

import keras
import tensorflow as tf
gpu_options = tf.GPUOptions(allow_growth=True)
sess = tf.Session(config=tf.ConfigProto(gpu_options=gpu_options))
keras.backend.tensorflow_backend.set_session(sess)

from keras import backend as K
from keras.models import Sequential, Model
from keras.layers import Input, LSTM, RepeatVector, TimeDistributed
from keras.layers.core import Flatten, Dense, Lambda
from keras.optimizers import SGD, RMSprop, Adam
from keras import regularizers
from keras.callbacks import CSVLogger, EarlyStopping

from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler(feature_range = (0, 1))

# Select gpu
import os
if gpu < 3:
    os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
    os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

    from tensorflow.python.client import device_lib
    print(device_lib.list_local_devices())

def create_lstm_autoencoder(nb_features, 
    n_pre, 
    n_post,
    batch_size, 
    latent_dim,
    lr,
    penalty,
    dr):
    """
    Creates an LSTM Autoencoder (VAE). Returns Autoencoder, Encoder, Generator. 
    (All code by fchollet - see reference.)

    # Arguments
        nb_features: int.
        n_pre: int, input timestep dimension.
        batch_size: int.
        intermediate_dim: int, output shape of LSTM. 
        latent_dim: int, latent z-layer shape. 

    # References
        - [Building Autoencoders in Keras](https://blog.keras.io/building-autoencoders-in-keras.html)
    """

    inputs = Input(shape=(n_pre, nb_features))
    encoded = LSTM(latent_dim, dropout=dr, return_sequences=False)(inputs)

    decoded = RepeatVector(n_post)(encoded)
    decoded = LSTM(nb_features, dropout=dr, return_sequences=True)(decoded)
    output = TimeDistributed(Dense(1, kernel_regularizer=regularizers.l2(penalty), name='Dense'), name='Outputs')(decoded)

    sequence_autoencoder = Model(inputs, output)
    encoder = Model(inputs, encoded)

    sequence_autoencoder.compile(optimizer=Adam(lr=lr), loss='mean_squared_error')
    return sequence_autoencoder

def get_data():
    # read data from file

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)
                
    x_obs = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))
    x_scaled = scaler.fit_transform(x_obs)

    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))
    y_scaled = scaler.fit_transform(y)

    print('raw x shape', x_scaled.shape) 
    print('raw y shape', y_scaled.shape)  

    dXC, dXT = [], []
    for i in range(seq_len-n_pre):
        dXC.append(x_scaled[i:i+n_pre]) 
        dXT.append(y_scaled[i:i+n_pre]) 
    return np.array(dXC),np.array(dXT),n_pre,n_post     

if __name__ == "__main__":
    x, y, n_pre, n_post = get_data() 

    print('x shape', x.shape)  
    print('y shape', y.shape)  

    nb_features = x.shape[2]
    batch_size = int(nb_batches)

    sequence_autoencoder = create_lstm_autoencoder(nb_features, 
        n_pre=n_pre, 
        n_post=n_post,
        batch_size=batch_size, 
        latent_dim=200,
        lr = lr,
        penalty=penalty,
        dr=dr)

    stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=50, verbose=0, mode='auto')

    csv_logger = CSVLogger('results/rae/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=False)

    sequence_autoencoder.fit(x, x, 
        epochs=int(epochs),
        verbose=1,
        callbacks=[stopping,csv_logger],
        validation_split=0.1)

    # now test 
    print('Generate predictions on test set')

    preds_test = sequence_autoencoder.predict(y, batch_size=batch_size, verbose=0)
    print('predictions shape =', preds_test.shape)
    preds_test = np.squeeze(preds_test)
    print('predictions shape =', preds_test.shape)

    preds_test = scaler.inverse_transform(preds_test) # reverse scaled preds to actual values
    print('predictions shape =', preds_test.shape)

    print('Saving to results/rae/{}/rae-{}-test.csv'.format(dataname,dataname))

    np.savetxt("results/rae/{}/rae-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")