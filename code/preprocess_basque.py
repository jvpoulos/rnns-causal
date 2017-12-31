import sys
import pandas as pd
import numpy as np
import cPickle as pkl

folder= sys.argv[-1] # 'treated'

## Read data

# basque
print('Reading data in data/basque/{}'.format(folder))

basque_x_train = pd.read_csv("data/basque/{}/basque-x-train.csv".format(folder)) 
basque_x_test = pd.read_csv("data/basque/{}/basque-x-test.csv".format(folder)) 

basque_y_train = pd.read_csv("data/basque/{}/basque-y-train.csv".format(folder)) 
basque_y_test = pd.read_csv("data/basque/{}/basque-y-test.csv".format(folder)) 

## Save train and test sets to disk
print('Saving data in data/basque/{}'.format(folder))

# basque

pkl.dump(basque_x_train, open('data/basque_x_train_treated.np', 'wb')) 
pkl.dump(basque_x_test, open('data/basque_x_test_treated.np', 'wb'))

pkl.dump(basque_y_train, open('data/basque_y_train_treated.np', 'wb'))
pkl.dump(basque_y_test, open('data/basque_y_test_treated.np', 'wb'))