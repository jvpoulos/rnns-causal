library(keras)
library(reticulate)
library(readr)
use_python("/usr/local/bin/python")

setwd("/media/jason/Dropbox/github/rnns-causal")

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
py$EPOCHS <- 1000
py$gpu <- 0
py$t0 <- t0
py$T <- T

source_python("code/train_lstm.py")

source_python("code/predict_lstm.py")

lstm.pred.control <- as.matrix(read_csv(paste0(results.directory, "lstm/basque/control/control-weights.1000-5.678.hdf5-test.csv), col_names = FALSE))