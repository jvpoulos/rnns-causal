###################################
# Synth Simulations: California #
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
names(synth.control.covars$california.xz) <- 1:length(synth.control.covars$california.xz)
california.covars.x <- t(as.matrix(bind_rows(synth.control.covars$california.xz))) # N x # predictors
colnames(california.covars.x) <- 1:ncol(california.covars.x)

print(paste0("N X T dimensions :", dim(synth.control.outcomes$california$M)))

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes,covars.x=california.covars.x,d='california',sim=0)