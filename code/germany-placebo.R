###################################
# Synth Simulations: Germany #
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
names(synth.control.covars$germany.xz) <- 1:length(synth.control.covars$germany.xz)
germany.covars.x <- t(as.matrix(bind_rows(lapply(synth.control.covars$germany.xz,colMeans)))) # N x # predictors
colnames(germany.covars.x) <- 1:ncol(germany.covars.x)

preProcValues <- preProcess(germany.covars.x, method = c("nzv","center","scale")) # preprocess
germany.covars.xt <- predict(preProcValues, germany.covars.x)

germany.covars.z <- Reduce(`+`,synth.control.covars$germany.xz)/length(synth.control.covars$germany.xz) # T x #predictors
colnames(germany.covars.z) <- gsub('.{2}$', '', colnames(germany.covars.z))

preProcValues <- preProcess(germany.covars.z, method = c("nzv","center","scale")) # preprocess
germany.covars.zt <- predict(preProcValues, germany.covars.z)

colnames(germany.covars.xt) <- colnames(germany.covars.zt)
rownames(germany.covars.zt) <- 1:nrow(germany.covars.zt)

## Run simulations
source("code/SynthSim.R")
SynthSim(outcomes=synth.control.outcomes,covars.x=germany.covars.xt, covars.z=germany.covars.zt,d='germany',sim=1)