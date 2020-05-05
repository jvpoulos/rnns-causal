###################################
# RVAE for Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
# use_python("/usr/local/bin/python")
use_python("~/venv/bin/python") # comet

rvae <- function(Y,treat_indices,d, t0, T){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N

  # Splits
  train_data <- data[,(-treat_indices)] # train on control units

  test_data <- data[,(treat_indices)] # treated units

  write.csv(train_data,paste0("data/",d,"-x.csv"),row.names = FALSE)
  write.csv(test_data,paste0("data/",d,"-y.csv"),row.names = FALSE)
  
  py <- import_main()
  py$dataname <- d
  py$t0 <- t0
  py$T <- T
  py$gpu <- 3
  py$epochs <- 5000
  py$patience <- 100
  py$lr <- 0.001
  py$dr <- 0.5
  py$nb_batches <- 1
  py$penalty <- 0.2

  source_python("code/train_rvae_sim.py")
  
  rvae.pred.test <- as.matrix(read_csv(paste0("results/rvae/",d,"/rvae-",d,"-test.csv"), col_names = FALSE))
  
  return(t(rvae.pred.test))  # N x T
}