###################################
# RVAE for Synth Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
use_python("/usr/local/bin/python")

rvae <- function(Y_obs,Y,treat_indices,d, t0, T){
  # Converting the data to a floating point matrix
  data_obs <- data.matrix(t(Y_obs)) # T x N
  data_truth <- data.matrix(t(Y)) # T x N
  
  # Splits
  train_data <- data_obs[,(-treat_indices)] # train on control units
  
  test_data <- data_truth[,(treat_indices)] # treated units
  
  write.csv(train_data,paste0("data/",d,"-x.csv"),row.names = FALSE)
  write.csv(test_data,paste0("data/",d,"-y.csv"),row.names = FALSE)
  
  py <- import_main()
  py$dataname <- d
  py$t0 <- t0
  py$T <- T
  py$patience <- 100
  py$nb_batches <- 1
  py$gpu <- 3
  py$epochs <- 10000
  py$lr <- 0.001
  py$penalty <- 0.01
  py$dr <- 0.5
  
  source_python("code/train_rvae_sim.py")
  
  rvae.pred <- as.matrix(read_csv(paste0("results/rvae/",d,"/rvae-",d,"-test.csv"), col_names = FALSE))
  
  return(t(rvae.pred))
}
