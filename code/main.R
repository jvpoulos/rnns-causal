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

## Placebo tests on synth data

source('/media/jason/Dropbox/github/land-reform/code/prepare-synth.R') # land-reford/data/synth-control-outcomes.rds

# synth-placebo.sh --> synth-placebo.R 

source(paste0(code.directory, "synth-placebo-plot.R"))

# Causal impact estimates: public education spending
# "/media/jason/Dropbox/github/land-reform/data/capacity-outcomes.rds"

source(paste0(code.directory, "prepare-funds.R"))

# funds-placebo.sh --> funds-placebo.R 
source(paste0(code.directory, "synth-placebo-plot.R"))

# train_encoder_decoder.py 
# train_rvae.py 

source(paste0(code.directory, "educ-plots.R"))