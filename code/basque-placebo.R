###################################
# Synth Simulations: Basque #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(dplyr)
library(glmnet)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
synth.control.outcomes <- readRDS("data/synth-control-outcomes.rds")

print(paste0("N X T dimensions :", dim(synth.control.outcomes$basque$M)))

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes, d='basque', sim=1)