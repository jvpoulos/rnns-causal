###################################
# Synth Simulations: California #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(dplyr)
library(caret)

# Load data
synth.control.outcomes <- readRDS("data/synth-control-outcomes.rds")
synth.control.covars <- readRDS("data/synth-control-covars.rds")

## Preprocess covariates
names(synth.control.covars$california.xz) <- 1:length(synth.control.covars$california.xz)
california.covars.x <- t(as.matrix(bind_rows(lapply(synth.control.covars$california.xz,colMeans)))) # N x # predictors
colnames(california.covars.x) <- 1:ncol(california.covars.x)

preProcValues <- preProcess(california.covars.x, method = c("nzv","center","scale")) # preprocess
california.covars.xt <- predict(preProcValues, california.covars.x)

california.covars.z <- Reduce(`+`,synth.control.covars$california.xz)/length(synth.control.covars$california.xz) # T x #predictors
colnames(california.covars.z) <- gsub('.{2}$', '', colnames(california.covars.z))

preProcValues <- preProcess(california.covars.z, method = c("nzv","center","scale")) # preprocess
california.covars.zt <- predict(preProcValues, california.covars.z)

colnames(california.covars.xt) <- colnames(california.covars.zt)
rownames(california.covars.zt) <- 1:nrow(california.covars.zt)

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes,covars.x=california.covars.xt, covars.z=california.covars.zt,d='california',sim=1)