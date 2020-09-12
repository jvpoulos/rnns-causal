# #!/bin/bash
# #----------------------------------------------------

# # By imputation method
# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'linear'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'linear' 

# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'locf'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'locf' 

# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 148 'none'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 148 'none' 

# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'median'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'median' 

# python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'random'
# python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'random' 

# By configuration
# Hidden activation: relu, tanh, sigmoid
# No. hidden: 128, 64, 32
# Patience: 20, 10, 0

python code/train_encoder_decoder.py 3 'relu' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 64 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 64 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 64 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 64 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 64 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 32 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 32 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 32 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 32 20 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 64 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 64 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 64 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 64 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 64 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 32 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 32 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 32 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 32 10 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 64 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 64 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 64 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 64 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 64 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 

python code/train_encoder_decoder.py 3 'relu' 32 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'relu' 32 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'tanh' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'tanh' 32 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 
python code/train_encoder_decoder.py 3 'sigmoid' 128 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd'
python code/train_lstm.py 3 'sigmoid' 32 0 0.7 2.0 0.001 500 32 'educ' 28 156 'svd' 