from __future__ import print_function

import numpy as np
import pandas as pd

import keras
import tensorflow as tf

from keras import backend as K
from keras.models import Sequential, Model
from keras.layers import Input, LSTM, RepeatVector
from keras.layers.core import Flatten, Dense, Lambda
from keras.optimizers import SGD, RMSprop, Adam
from keras import regularizers
from keras.callbacks import CSVLogger, EarlyStopping

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

def create_lstm_vae(nb_features, 
    n_pre, 
    n_post,
    batch_size, 
    intermediate_dim, 
    latent_dim,
    lr,
    penalty,
    dr,
    epsilon_std=1.):

    """
    Creates an LSTM Variational Autoencoder (VAE). Returns VAE, Encoder, Generator. 

    # Arguments
        nb_features: int.
        n_pre: int, input timestep dimension.
        batch_size: int.
        intermediate_dim: int, output shape of LSTM. 
        latent_dim: int, latent z-layer shape. 
        epsilon_std: float, z-layer sigma.


    # References
        - [Building Autoencoders in Keras](https://blog.keras.io/building-autoencoders-in-keras.html)
        - [Generating sentences from a continuous space](https://arxiv.org/abs/1511.06349)
    """

    x = Input(shape=(n_pre, nb_features), name='Encoder_inputs')
    weights_tensor = Input(shape=(n_pre, nb_features), name="Weights")

    # LSTM encoding
    h = LSTM(intermediate_dim, dropout=dr, name='Encoder')(x)

    # VAE Z layer
    z_mean = Dense(latent_dim, name='z_mean')(h)
    z_log_sigma = Dense(latent_dim, name='z_log_sigma')(h)
    
    def sampling(args):
        z_mean, z_log_sigma = args
        epsilon = K.random_normal(shape=(batch_size, latent_dim),
                                  mean=0., stddev=epsilon_std)
        return z_mean + K.exp(z_log_sigma) * epsilon

    # note that "output_shape" isn't necessary with the TensorFlow backend
    # so you could write `Lambda(sampling)([z_mean, z_log_sigma])`
    z = Lambda(sampling, output_shape=(latent_dim,), name='Sampling')([z_mean, z_log_sigma])
    
    # decoded LSTM layer
    decoder_h = LSTM(intermediate_dim, dropout=dr, return_sequences=True, name='Decoder_1')
    decoder_mean = LSTM(nb_features, dropout=dr, return_sequences=True, name='Decoder_2')

    h_decoded = RepeatVector(n_post, name='Repeat')(z)
    h_decoded = decoder_h(h_decoded)

    # decoded layer
    x_decoded_mean = decoder_mean(h_decoded)
    
    # end-to-end autoencoder
    vae = Model([x, weights_tensor], x_decoded_mean)

    # encoder, from inputs to latent space
    encoder = Model(x, z_mean)

    # generator, from latent space to reconstructed inputs
    decoder_input = Input(shape=(latent_dim,))

    _h_decoded = RepeatVector(n_post)(decoder_input)
    _h_decoded = decoder_h(_h_decoded)

    _x_decoded_mean = decoder_mean(_h_decoded)
    generator = Model(decoder_input, _x_decoded_mean)
    
    def vae_loss(x, x_decoded_mean, weights):
        xent_loss = weighted_mse(x, x_decoded_mean, weights)
        kl_loss = - 0.5 * K.mean(1 + z_log_sigma - K.square(z_mean) - K.exp(z_log_sigma))
        loss = xent_loss + kl_loss
        return loss

    cl = wrapped_partial(vae_loss, weights=weights_tensor)

    vae.compile(optimizer=Adam(lr=lr), loss=cl)
    
    return vae, encoder, generator

def get_data():
    # read data from file

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)

    wx = np.array(pd.read_csv("data/{}-wx.csv".format(dataname)))  

    x_obs = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))
    x_scaled = scaler.fit_transform(x_obs)

    wy = np.array(pd.read_csv("data/{}-wy.csv".format(dataname)))    

    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))
    y_scaled = scaler.fit_transform(y)

    print('raw x shape', x_scaled.shape) 
    print('raw y shape', y_scaled.shape)  

    dXC,  wXC, dXT,  wXT  = [], [], [], []
    for i in range(seq_len-n_pre):
        dXC.append(x_scaled[i:i+n_pre]) # controls
        wXC.append(wx[i:i+n_pre]) 
        dXT.append(y_scaled[i:i+n_pre]) # treated 
        wXT.append(wy[i:i+n_pre]) 
    return np.array(dXC),np.array(wXC),np.array(dXT),np.array(wXT),n_pre,n_post      

if __name__ == "__main__":
    x, wx, y, wy, n_pre, n_post = get_data() 
    nb_features = x.shape[2]
    batch_size = int(nb_batches)

    vae, enc, gen = create_lstm_vae(nb_features, 
        n_pre=n_pre, 
        n_post=n_post,
        batch_size=batch_size, 
        intermediate_dim=32,
        latent_dim=200,
        lr = lr,
        penalty=penalty,
        dr=dr,
        epsilon_std=1.)

    stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=int(patience), verbose=0, mode='auto')

    csv_logger = CSVLogger('results/rvae/{}/training_log_{}.csv'.format(dataname,dataname), separator=',', append=False)

    vae.fit([x,wx], x, 
        epochs=int(epochs),
        verbose=1,
        callbacks=[stopping,csv_logger],
        validation_split=0.1)

    # now test 
    print('Generate predictions on test set')

    preds_test = vae.predict([wy,y], batch_size=batch_size, verbose=0)
    print('predictions shape =', preds_test.shape)
    preds_test = np.squeeze(preds_test)
    print('predictions shape (squeezed) =', preds_test.shape)

    preds_test = scaler.inverse_transform(preds_test) # reverse scaled preds to actual values
    print('predictions shape (transformed)=', preds_test.shape)

    print('Saving to results/rvae/{}/rvae-{}-test.csv'.format(dataname,dataname))

    np.savetxt("results/rvae/{}/rvae-{}-test.csv".format(dataname,dataname), preds_test, delimiter=",")