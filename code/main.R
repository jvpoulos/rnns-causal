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
# stock-placebo.sh ---> stock-placebo.R 
# stock-placebo-fixed.sh ---> stock-placebo-fixed.R 

figures <- FALSE
if(figures){
  source("synth-placebo-plot.R")
  source("educ-placebo-plot.R")  
  source("stock-placebo-plot.R")
}

# Causal impact estimates: public education spending

source("prepare-funds-locf.R")

# train_lstm.py 
# train_encoder_decoder.py 
# train_rvae.py 
if(figures){
  source( "educ-plot.R")
}