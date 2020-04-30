###################################
# Main (rnns-causal)              #
###################################

# Setup parallel processing 
require(parallel)
require(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

## Placebo tests

# basque-placebo.sh ---> basque-placebo.R 
# california-placebo.sh ---> california-placebo.R 
# germany-placebo.sh ---> germany-placebo.R 
# educ-placebo.sh ---> educ-placebo.R 
# covid-placebo.sh ---> covid-placebo.R 
# stock-placebo.sh ---> stock-placebo.R 

figures <- FALSE
if(figures){
  source("stock-placebo-plot.R")
}

# Causal impact estimates: public education spending

source("prepare-funds.R")

# train_lstm.py 
# train_encoder_decoder.py 
# train_rvae.py 
if(figures){
  source( "educ-plot.R")
}