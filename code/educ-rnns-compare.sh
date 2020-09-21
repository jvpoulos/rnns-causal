# #!/bin/bash
# #----------------------------------------------------

# # By imputation method
# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'ma'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'ma' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'linear'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'linear' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 148 'none'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 148 'none' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'mean'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'mean' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'random'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'random' 

# By configuration
# Hidden activation: relu, tanh, sigmoid
# No. hidden: 256, 128, 64
# Patience: 50, 25, 0
# Dropout: 0.5, 0.2, 0

#<GPU_ID> <hidden_activation>  <n_hidden> <patience> <dropout rate> <penalty> <learning_rate> <epochs> <batches> <data_name> <window_size> <T> <imputation_method>;

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 50 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

# #### 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 50 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0.5 0.001 0.001 500 32 'educ' 20 159 'locf' 

# ##

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 50 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

###

# python code/train_encoder_decoder.py 3 'tanh' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 25 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 256 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 64 0 0 0.001 0.001 500 32 'educ' 20 159 'locf' 

# python code/train_encoder_decoder.py 3 'tanh' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'tanh' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'relu' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'relu' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
# python code/train_encoder_decoder.py 3 'sigmoid' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
# python code/train_lstm.py 3 'sigmoid' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 

python code/train_encoder_decoder.py 3 'tanh' 256 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
python code/train_lstm.py 3 'tanh' 256 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
python code/train_encoder_decoder.py 3 'relu' 256 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
python code/train_lstm.py 3 'relu' 256 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 
python code/train_encoder_decoder.py 3 'sigmoid' 256 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
python code/train_lstm.py 3 'sigmoid' 256 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf' 