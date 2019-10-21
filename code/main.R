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

source("prepare-synth.R") # ---> data/synth-control-outcomes.rds
source("elections.R") # --> data/elections/votediff.csv

# elections-placebo.sh  --> elections-placebo.R
# stock-placebo.sh ---> stock-placebo.R 
# educ-placebo.sh ---> educ-placebo.R 
# synth-placebo.sh ---> synth-placebo.R 

figures <- FALSE
if(figures){
  source("synth-placebo-plot.R")
  source("stock-placebo-plot.R")
  source("funds-placebo-plot.R")
}

# Causal impact estimates: public education spending

source("prepare-funds-locf.R")
source("prepare-funds-linear.R") #imputation sensitivity
source("prepare-funds-random.R")
source("prepare-funds-median.R")

# train_encoder_decoder.py 
# train_rvae.py 
if(figures){
  source( "educ-plot.R")
}