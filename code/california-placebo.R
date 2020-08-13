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
california.covars.x <- t(as.matrix(bind_rows(lapply(synth.control.covars$california.xz,colMeans)))) # N x # predictors
colnames(california.covars.x) <- 1:ncol(california.covars.x)

california.covars.z <- Reduce(`+`,synth.control.covars$california.xz)/length(synth.control.covars$california.xz) # T x #predictors
colnames(california.covars.z) <- gsub('.{2}$', '', colnames(california.covars.z))

colnames(california.covars.x) <- colnames(california.covars.z)
rownames(california.covars.z) <- 1:nrow(california.covars.z)

print(paste0("N X T dimensions :", dim(synth.control.outcomes$california$M)))

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes,covars.x=california.covars.x,d='california',sim=0)