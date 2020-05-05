from __future__ import print_function

import os.path
from os import path

import sys
import math
import numpy as np
import pandas as pd

from keras import backend as K
from keras.models import Sequential, Model
from keras.layers import Input, Masking, LSTM, RepeatVector
from keras.layers.core import Flatten, Dense, Lambda
from keras.optimizers import SGD, RMSprop, Adam
from keras import regularizers
from keras.callbacks import CSVLogger, EarlyStopping, TerminateOnNaN

from sklearn.preprocessing import StandardScaler, MinMaxScaler
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
gpu = sys.argv[-11]
if gpu < 3:
    os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
    os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

    from tensorflow.python.client import device_lib
    print(device_lib.list_local_devices())

imp = sys.argv[-1]
T = sys.argv[-2]
t0 = sys.argv[-3]
dataname = sys.argv[-4] 
nb_batches = sys.argv[-5]
nb_epochs = sys.argv[-6]
lr = float(sys.argv[-7])
penalty = float(sys.argv[-8])
dr = float(sys.argv[-9])
patience = sys.argv[-10]

# Create directories
results_directory = 'results/rvae/{}'.format(dataname)
data_directory = 'data/{}'.format(dataname)

if not os.path.exists(results_directory):
    os.makedirs(results_directory)

if not os.path.exists(data_directory):
    os.makedirs(data_directory)

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

    x = Input(shape=(n_pre, nb_features), name="Inputs")
    mask = Masking(mask_value=0.)(x)

    x = Masking(mask_value=0., input_shape=(n_pre, nb_features), name='Encoder_inputs')
    weights_tensor = Input(shape=(n_pre, nb_features), name="Weights")

    # LSTM encoding
    h = LSTM(intermediate_dim, dropout=dr, name='Encoder')(mask)

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

    wx = np.array(pd.read_csv("data/{}-wx-{}.csv".format(dataname,imp)))  
             
    x = np.array(pd.read_csv("data/{}-x-{}.csv".format(dataname,imp)))
    x_scaled = scaler.fit_transform(x)

    print('raw x shape', x_scaled.shape)   

    wy = np.array(pd.read_csv("data/{}-wy-{}.csv".format(dataname,imp)))    

    y = np.array(pd.read_csv("data/{}-y-{}.csv".format(dataname,imp)))
    y_scaled = scaler.transform(y)

    print('raw y shape', y.shape) 

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

    # Load pre-trained weights
    weights_path = 'results/rvae/{}'.format(dataname) +'/weights-{}-{}.h5'.format(str(n_pre), str(nb_features))
    if path.exists(weights_path):
        print("loading weights from", weights_path)
        vae.load_weights(weights_path)    

    stopping = EarlyStopping(monitor='val_loss', patience=int(patience), min_delta=0.001, verbose=1, mode='min', restore_best_weights=True)

    csv_logger = CSVLogger('results/rvae/{}/training_log_{}_{}.csv'.format(dataname,dataname,imp), separator=',', append=False)

    terminate = TerminateOnNaN()

    vae.fit([x,wx], x, 
        epochs=int(nb_epochs),
        verbose=1,
        callbacks=[csv_logger,stopping,terminate],
        validation_split=0.1)

    # save weights
    vae.save_weights('results/rvae/{}'.format(dataname) +'/weights-{}-{}.h5'.format(str(n_pre),str(nb_features)))

    # now test

    print('Generate predictions on full training set')

    preds_train = vae.predict([wx,x], batch_size=batch_size, verbose=0)

    print('predictions shape =', preds_train.shape)
    preds_train = np.squeeze(preds_train)
    print('predictions shape (squeezed) =', preds_train.shape)

    preds_train = scaler.inverse_transform(preds_train) # reverse scaled preds to actual values
    print('predictions shape (transformed)=', preds_train.shape)

    print('Saving to results/rvae/{}/rvae-{}-train-{}.csv'.format(dataname,dataname,imp))

    np.savetxt("results/rvae/{}/rvae-{}-train-{}.csv".format(dataname,dataname,imp), preds_train, delimiter=",")

    print('Generate predictions on test set')

    print('y samples shape', y.shape)     
    print('wy samples shape', wy.shape)  

    preds_test = vae.predict([wy,y], batch_size=batch_size, verbose=0)
    print('predictions shape =', preds_test.shape)
    preds_test = np.squeeze(preds_test)
    print('predictions shape (squeezed) =', preds_test.shape)

    preds_test = scaler.inverse_transform(preds_test) # reverse scaled preds to actual values
    print('predictions shape (transformed)=', preds_test.shape)

    print('Saving to results/rvae/{}/rvae-{}-test-{}.csv'.format(dataname,dataname,imp))

    np.savetxt("results/rvae/{}/rvae-{}-test-{}.csv".format(dataname,dataname,imp), preds_test, delimiter=",")