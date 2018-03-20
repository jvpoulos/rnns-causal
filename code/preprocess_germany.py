import sys
import pandas as pd
import numpy as np
import cPickle as pkl

folder= sys.argv[-1] # 'treated'

## Read data

# germany
print('Reading data in data/germany/{}'.format(folder))

germany_x_train = pd.read_csv("data/germany/{}/germany-x-train.csv".format(folder)) 
germany_x_test = pd.read_csv("data/germany/{}/germany-x-test.csv".format(folder)) 

germany_y_train = pd.read_csv("data/germany/{}/germany-y-train.csv".format(folder)) 
germany_y_test = pd.read_csv("data/germany/{}/germany-y-test.csv".format(folder)) 

## Save train and test sets to disk
print('Saving data in data/germany/{}'.format(folder))

# germany

pkl.dump(germany_x_train, open('data/germany_x_train_treated.np', 'wb')) 
pkl.dump(germany_x_test, open('data/germany_x_test_treated.np', 'wb'))

pkl.dump(germany_y_train, open('data/germany_y_train_treated.np', 'wb'))
pkl.dump(germany_y_test, open('data/germany_y_test_treated.np', 'wb'))