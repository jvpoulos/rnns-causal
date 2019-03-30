import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from keras.utils.vis_utils import plot_model, model_to_dot
from keras.callbacks import ModelCheckpoint, CSVLogger

from lstm_vae import create_lstm_vae

# Select gpu
import sys
import os
gpu = sys.argv[-4]
os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID"   # see issue #152
os.environ["CUDA_VISIBLE_DEVICES"]= "{}".format(gpu)

from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())

analysis = sys.argv[-1] # 'treated' or 'control'
dataname = sys.argv[-3] 

def set_trace():
    from IPython.core.debugger import Pdb
    import sys
    Pdb(color_scheme='Linux').set_trace(sys._getframe().f_back)

def get_data():
    # read data from file

    if dataname == 'basque':
        n_post = 1 
        n_pre = 14-1
        seq_len = 43

    if dataname == 'california':
        n_post  = 1 
        n_pre =  19-1 
        seq_len = 31

    if dataname == 'germany':
        n_post  = 1 
        n_pre =  30-1 
        seq_len = 44   

    if dataname == 'west-revpc':
        n_post  = 1 
        n_pre =  52-1 
        seq_len = 119  

    if dataname == 'west-exppc':
        n_post  = 1 
        n_pre =  51-1 
        seq_len = 117  

    if dataname == 'west-educpc':
        n_post  = 1 
        n_pre =  37-1 
        seq_len = 99  

    if dataname == 'south-revpc':
        n_post  = 1 
        n_pre =  36-1 
        seq_len = 97  

    if dataname == 'south-exppc':
        n_post  = 1 
        n_pre =  37-1 
        seq_len = 98  

    if dataname == 'south-educpc':
        n_post  = 1 
        n_pre =  33-1 
        seq_len = 90 
                
    y = np.array(pd.read_csv("data/{}-y.csv".format(dataname)))
    x = np.array(pd.read_csv("data/{}-x.csv".format(dataname)))  
    
    if analysis == 'treated-gans': 
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
    dr=0.5
    penalty=0.001
    lr=0.001
    period=10 # checkpoint period

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

    # Load weights
    filename = sys.argv[-2]
    vae.load_weights(filename, by_name=True)

    print("Created model and loaded weights from file")

    preds = vae.predict(x, batch_size=batch_size, verbose=1)

    preds = np.squeeze(preds)

    print('predictions shape =', preds.shape)

    np.savetxt("{}-{}-test.csv".format(filename,dataname), preds, delimiter=",")

    # Visualize model

    plot_model(vae, to_file='results/vae.png', # Plot graph of model
    show_shapes = False,
    show_layer_names = True)

    plot_model(enc, to_file='results/enc.png', # Plot graph of model
    show_shapes = False,
    show_layer_names = True)

    plot_model(gen, to_file='results/gen.png', # Plot graph of model
    show_shapes = False,
    show_layer_names = True)