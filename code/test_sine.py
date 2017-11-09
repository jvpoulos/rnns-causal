from attrdict import AttrDict
from keras.models import load_model
import numpy as np
import matplotlib.pyplot as plt
import sys

from keras.models import Sequential
from keras.layers import Dense, Activation, LSTM, Dropout, RepeatVector, TimeDistributed

EPOCHS = 10

def create_model(steps_before, steps_after, feature_count):
    """ 
        creates, compiles and returns a RNN model 
        @param steps_before: the number of previous time steps (input)
        @param steps_after: the number of posterior time steps (output or predictions)
        @param feature_count: the number of features in the model
        @param hidden_neurons: the number of hidden neurons per LSTM layer
    """
    DROPOUT = 0.5
    LAYERS = 2
    
    hidden_neurons = 300

    model = Sequential()
    model.add(LSTM(input_dim=feature_count, output_dim=hidden_neurons, return_sequences=False))
    model.add(RepeatVector(steps_after))
    model.add(LSTM(output_dim=hidden_neurons, return_sequences=True))
    model.add(TimeDistributed(Dense(feature_count)))
    model.add(Activation('linear'))  
    
    model.compile(loss='mean_squared_error', optimizer='rmsprop', metrics=['accuracy'])  
    return model


def train_sinus(model, dataX, dataY, epoch_count):
    """ 
        trains only the sinus model
    """
    history = model.fit(dataX, dataY, batch_size=1, nb_epoch=epoch_count, validation_split=0.05)

def test_sinus():
    ''' 
        testing how well the network can predict
        a simple sinus wave.
    '''
    t = np.arange(0.0, 4.0, 0.02)
    sinus = np.sin(2 * np.pi * t)
    sinus = sinus.reshape((sinus.shape[0], 1))
    n_pre = 50
    n_post = 10
    
    dX, dY = [], []
    for i in range(len(sinus)-n_pre-n_post):
        dX.append(sinus[i:i+n_pre])
        dY.append(sinus[i+n_pre:i+n_pre+n_post])
        #dY.append(sinus[i+n_pre])
    dataX = np.array(dX)
    dataY = np.array(dY)

    # create and fit the LSTM network
    print('creating model...')
    model = create_model(n_pre, n_post, 1)
    train_sinus(model, dataX, dataY, EPOCHS)
    
    # now test
    t = np.arange(15.0, 19.0, 0.02)
    sinus = np.sin(2 * np.pi * t)
    sinus = sinus.reshape((sinus.shape[0], 1))
    
    dX, dY = [], []
    for i in range(len(sinus)-n_pre-n_post):
        dX.append(sinus[i:i+n_pre])
        dY.append(sinus[i+n_pre:i+n_pre+n_post])
    dataX = np.array(dX)
    dataY = np.array(dY)
    
    predict = model.predict(dataX)
    
    # now plot
    nan_array = np.empty((n_pre - 1))
    nan_array.fill(np.nan)
    nan_array2 = np.empty(n_post)
    nan_array2.fill(np.nan)
    ind = np.arange(n_pre + n_post)

    fig, ax = plt.subplots()
    for i in range(0, 50, 50):

        forecasts = np.concatenate((nan_array, dataX[i, -1:, 0], predict[i, :, 0]))
        ground_truth = np.concatenate((nan_array, dataX[i, -1:, 0], dataY[i, :, 0]))
        network_input = np.concatenate((dataX[i, :, 0], nan_array2))
     
        ax.plot(ind, network_input, 'b-x', label='Network input')
        ax.plot(ind, forecasts, 'r-x', label='Many to many model forecast')
        ax.plot(ind, ground_truth, 'g-x', label = 'Ground truth')
        
        plt.xlabel('t')
        plt.ylabel('sin(t)')
        plt.title('Sinus Many to Many Forecast')
        plt.legend(loc='best')
        plt.savefig('test_sinus/plot_mtm_triple_' + str(i) + '.png')
        plt.cla()

def main():
    test_sinus()
    return 1

if __name__ == "__main__":
    sys.exit(main())