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
synth.control.covars <- readRDS("data/synth-control-covars.rds")

## Preprocess covariates
names(synth.control.covars$basque.xz) <- 1:length(synth.control.covars$basque.xz)
basque.covars.x <- t(as.matrix(bind_rows(synth.control.covars$basque.xz))) # N x # predictors
colnames(basque.covars.x) <- 1:ncol(basque.covars.x)

print(paste0("N X T dimensions :", dim(synth.control.outcomes$basque$M)))

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes, covars.x=basque.covars.x, d='basque', sim=1)