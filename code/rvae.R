###################################
# RVAE for Synth Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
use_python("/usr/local/bin/python")

# Converting the data to a floating point matrix
data <- data.matrix(t(Y_obs)) # T x N

# Splits
train_data <- data[,(-treat_indices)] # train on control units

test_data <- data[,(treat_indices)] # treated units

write.csv(train_data,paste0("data/",d,"-x.csv"),row.names = FALSE)
write.csv(test_data,paste0("data/",d,"-y.csv"),row.names = FALSE)

py <- import_main()
py$dataname <- d
py$analysis <- 'control'
py$epochs <- 10000
py$gpu <- 0
py$t0 <- t0
py$T <- T

source_python("code/train_rvae.py")

rvae.pred.control <- as.matrix(read_csv("results/rvae/basque/control/rvae-control-basque-test.csv", col_names = FALSE))