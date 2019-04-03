from __future__ import print_function

import numpy as np
import pandas as pd

import keras
from keras import backend as K
from keras.models import Sequential, Model
from keras.layers import Input, LSTM, RepeatVector
from keras.layers.core import Flatten, Dense, Dropout, Lambda
from keras.optimizers import SGD, RMSprop, Adam
from keras import regularizers
from keras import objectives
from keras.callbacks import CSVLogger, EarlyStopping

# Select gpu
import os
#gpu = sys.argv[-4]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

# analysis = sys.argv[-1] # 'treated' or 'control'
# dataname = sys.argv[-2] 

# epoches = int(sys.argv[-3])

def create_lstm_vae(nb_features, 
    n_pre, 
    n_post,
    batch_size, 
    intermediate_dim, 
    latent_dim,
    initialization,
    activation,
    lr,
    penalty,
    dropout,
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

    # LSTM encoding
    h = LSTM(intermediate_dim, kernel_initializer=initialization, name='Encoder')(x)

    # VAE Z layer
    z_mean = Dense(latent_dim, name='z_mean')(h)
    z_log_sigma = Dense(latent_dim, name='z_log_sigma')(h)
    
    def sampling(args):
        z_mean, z_log_sigma = args
        epsilon = K.random_normal(shape=(batch_size, latent_dim),
                                  mean=0., stddev=epsilon_std)
        epsilon = K.exp(epsilon) # log-normal sampling
        # return z_mean + K.exp(z_log_sigma) * epsilon
        return z_mean + z_log_sigma * epsilon

    # note that "output_shape" isn't necessary with the TensorFlow backend
    # so you could write `Lambda(sampling)([z_mean, z_log_sigma])`
    z = Lambda(sampling, output_shape=(latent_dim,), name='Sampling')([z_mean, z_log_sigma])
    
    # decoded LSTM layer
    decoder_h = LSTM(intermediate_dim, kernel_initializer=initialization, return_sequences=True, name='Decoder_1')
    decoder_mean = LSTM(nb_features, kernel_initializer=initialization, activation=activation, kernel_regularizer=regularizers.l2(penalty), return_sequences=True, name='Decoder_2')

    h_decoded = RepeatVector(n_post, name='Repeat')(z)
    h_decoded = decoder_h(h_decoded)

    # decoded layer
    x_decoded_mean = decoder_mean(h_decoded)
    
    # end-to-end autoencoder
    vae = Model(x, x_decoded_mean)

    # encoder, from inputs to latent space
    encoder = Model(x, z_mean)

    # generator, from latent space to reconstructed inputs
    decoder_input = Input(shape=(latent_dim,))

    _h_decoded = RepeatVector(n_post)(decoder_input)
    _h_decoded = decoder_h(_h_decoded)

    _x_decoded_mean = decoder_mean(_h_decoded)
    generator = Model(decoder_input, _x_decoded_mean)
    
    def vae_loss(x, x_decoded_mean):
        xent_loss = objectives.mse(x, x_decoded_mean)
        kl_loss = - 0.5 * K.mean(1 + z_log_sigma - K.square(z_mean) - K.exp(z_log_sigma))
        loss = xent_loss + kl_loss
        return loss

    vae.compile(optimizer=Adam(lr=lr), loss=vae_loss)
    
    return vae, encoder, generator

def get_data():
    # read data from file

    n_post = int(1)
    n_pre =int(t0)-1
    seq_len = int(T)
                
    y = np.array(pd.read_csv("../data/{}-y.csv".format(dataname)))
    x = np.array(pd.read_csv("../data/{}-x.csv".format(dataname)))

    if analysis == 'treated': 
        print('raw x shape', x.shape)    

        dX = []
        for i in range(seq_len-n_pre-n_post):
            dX.append(y[i:i+n_pre]) # treated is input
        return np.array(dX), n_pre, n_post

    if analysis == 'control': 

        print('raw x shape', x.shape)   

        dX = []
        for i in range(seq_len-n_pre-n_post):
            dX.append(x[i:i+n_pre]) # controls are inputs
        return np.array(dX), n_pre, n_post       

if __name__ == "__main__":
    x, n_pre, n_post = get_data() 
    nb_features = x.shape[2]
    batch_size = 1
    penalty=0.001
    lr=0.001
    dr=0.5

    if dataname == 'germany':
        penalty=0
        lr=0.00005

    vae, enc, gen = create_lstm_vae(nb_features, 
        n_pre=n_pre, 
        n_post=n_post,
        batch_size=batch_size, 
        intermediate_dim=32,
        latent_dim=200,
        initialization = 'glorot_normal',
        activation = 'linear',
        lr = lr,
        penalty=penalty,
        dropout=dr,
        epsilon_std=1.)

    stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=10, verbose=1, mode='auto')

    csv_logger = CSVLogger('../results/rvae/{}/{}/training_log_{}_{}.csv'.format(dataname,analysis,dataname,analysis), separator=',', append=False)

    vae.fit(x, x, 
        epochs=int(epochs),
        verbose=1,
        callbacks=[stopping,csv_logger],
        validation_split=0.2)

	# now test

    print('Generate predictions')

    preds = vae.predict(x, batch_size=batch_size, verbose=1)

    preds = np.squeeze(preds)

    print('predictions shape =', preds.shape)

    print('Saving to results/rvae/{}/{}/rvae-{}-{}-test.csv'.format(dataname,analysis,analysis,dataname))

    np.savetxt("../results/rvae/{}/{}/rvae-{}-{}-test.csv".format(dataname,analysis,analysis,dataname), preds, delimiter=",")