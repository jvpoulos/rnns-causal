import sys
import pandas as pd
import numpy as np
import cPickle as pkl

folder= sys.argv[-1] # 'treated'

## Read data

# california
print('Reading data in data/california/{}'.format(folder))

california_x_train = pd.read_csv("data/california/{}/california-x-train.csv".format(folder)) 
california_x_test = pd.read_csv("data/california/{}/california-x-test.csv".format(folder)) 

california_y_train = pd.read_csv("data/california/{}/california-y-train.csv".format(folder)) 
california_y_test = pd.read_csv("data/california/{}/california-y-test.csv".format(folder)) 

## Save train and test sets to disk
print('Saving data in data/california/{}'.format(folder))

# california

pkl.dump(california_x_train, open('data/california_x_train_treated.np', 'wb')) 
pkl.dump(california_x_test, open('data/california_x_test_treated.np', 'wb'))

pkl.dump(california_y_train, open('data/california_y_train_treated.np', 'wb'))
pkl.dump(california_y_test, open('data/california_y_test_treated.np', 'wb'))