###################################
# Synth Simulations: W. Germany #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(dplyr)

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
synth.control.covars <- readRDS("data/synth-control-covars.rds")

## Preprocess covariates
names(synth.control.covars$germany.xz) <- 1:length(synth.control.covars$germany.xz)
germany.covars.x <- t(as.matrix(bind_rows(synth.control.covars$germany.xz))) # N x # predictors
colnames(germany.covars.x) <- 1:ncol(germany.covars.x)

print(paste0("N X T dimensions :", dim(synth.control.outcomes$germany$M)))

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes,covars.x=germany.covars.x,d='germany',sim=0)