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
data.directory <- "/media/jason/Dropbox/github/rnns-causal/data/"
code.directory <-"/media/jason/Dropbox/github/rnns-causal/code/"
results.directory <-"/media/jason/Dropbox/github/rnns-causal/results/"

## Placebo tests

source(paste0(code.directory, "prepare-synth.R")) # ---> data/synth-control-outcomes.rds

# synth-placebo.sh ---> synth-placebo.R 
# stock-placebo.sh ---> stock-placebo.R 
# funds-placebo.sh ---> funds-placebo.R 
figures <- FALSE
if(figures){
  source(paste0(code.directory, "synth-placebo-plot.R"))
  source(paste0(code.directory, "stock-placebo-plot.R"))
  source(paste0(code.directory, "funds-placebo-plot.R"))
}

# Causal impact estimates: public education spending

source(paste0(code.directory, "prepare-funds-locf.R"))
source(paste0(code.directory, "prepare-funds-linear.R")) #imputation sensitivity
source(paste0(code.directory, "prepare-funds-random.R"))
source(paste0(code.directory, "prepare-funds-median.R"))

# train_encoder_decoder.py 
# train_rvae.py 
if(figures){
  source(paste0(code.directory, "educ-plot.R"))
}