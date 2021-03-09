#!/bin/bash
#----------------------------------------------------

# Imputation sensitivity

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'ma'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'ma'

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'linear'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'linear'

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'mean'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'mean'

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'random'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'random'

# RNN configuration sensitivity

python3 code/train_encoder_decoder.py 3 'sigmoid' 256 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #1
python3 code/train_lstm.py 3 'sigmoid' 256 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 256 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #2
python3 code/train_lstm.py 3 'tanh' 256 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 128 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #3
python3 code/train_lstm.py 3 'sigmoid' 128 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 128 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #4
python3 code/train_lstm.py 3 'tanh' 128 50 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 256 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #5
python3 code/train_lstm.py 3 'sigmoid' 256 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 256 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #6
python3 code/train_lstm.py 3 'tanh' 256 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #7
python3 code/train_lstm.py 3 'sigmoid' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf' #8
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 256 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #9
python3 code/train_lstm.py 3 'sigmoid' 256 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 256 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #10
python3 code/train_lstm.py 3 'tanh' 256 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 128 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #11
python3 code/train_lstm.py 3 'sigmoid' 128 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 128 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #12
python3 code/train_lstm.py 3 'tanh' 128 50 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 256 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #13
python3 code/train_lstm.py 3 'sigmoid' 256 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 256 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #14
python3 code/train_lstm.py 3 'tanh' 256 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'sigmoid' 128 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #15
python3 code/train_lstm.py 3 'sigmoid' 128 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'

python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf' #16
python3 code/train_lstm.py 3 'tanh' 128 25 0.2 0.01 0.001 500 32 'educ' 10 203 'locf'