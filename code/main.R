###################################
# Main (rnns-causal)              #
###################################

# Setup parallel processing 
require(parallel)
require(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set directories
data.directory <- "~/Dropbox/github/rnns-causal/data/"
code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

source('utils.R')

## Placebo tests on synth data

source('/media/jason/Dropbox/github/land-reform/code/prepare-synth.R') 

# synth-placebo.sh --> synth-placebo.R 

# Causal impact estimates: public education spending

# funds_encoder_decoder.sh --> train_encoder_decoder.py 
# funds_encoder_decoder_predict.sh --> predict_encoder_decoder.py 

# funds_lstm.sh --> train_lstm.py 
# funds_lstm_predict.sh --> predict_lstm.py 

# funds_vae.sh --> train_vae.py 
# funds_vae_predict.sh --> predict_vae.py 