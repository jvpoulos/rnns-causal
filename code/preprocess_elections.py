import sys
import pandas as pd
import numpy as np
import cPickle as pkl

folder= sys.argv[-1] # 'treated'

## Read data

# elections
print('Reading data in data/elections/{}'.format(folder))

votediff_x_train = pd.read_csv("data/elections/{}/votediff-x-train.csv".format(folder)) 
votediff_x_test = pd.read_csv("data/elections/{}/votediff-x-test.csv".format(folder)) 

votediff_y_train = pd.read_csv("data/elections/{}/votediff-y-train.csv".format(folder)) 
votediff_y_test = pd.read_csv("data/elections/{}/votediff-y-test.csv".format(folder)) 

## Save train and test sets to disk
print('Saving data in data/elections/{}'.format(folder))

# elections
pkl.dump(votediff_x_train, open('data/votediff_x_train_treated.np', 'wb')) 
pkl.dump(votediff_x_test, open('data/votediff_x_test_treated.np', 'wb'))

pkl.dump(votediff_y_train, open('data/votediff_y_train_treated.np', 'wb'))
pkl.dump(votediff_y_test, open('data/votediff_y_test_treated.np', 'wb'))