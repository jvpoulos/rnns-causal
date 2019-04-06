###################################
# RVAE for Synth Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
use_python("/usr/local/bin/python")

rvae <- function(Y_obs,treat_indices,d, t0, T){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y_obs)) # T x N
  
  # Splits
  train_data <- data[,(-treat_indices)] # train on control units
  
  test_data <- data[,(treat_indices)] # treated units
  
  write.csv(train_data,paste0("../data/",d,"-x.csv"),row.names = FALSE)
  write.csv(test_data,paste0("../data/",d,"-y.csv"),row.names = FALSE)
  
  py <- import_main()
  py$dataname <- d
  py$epochs <- 1500
  py$gpu <- 0
  py$t0 <- t0
  py$T <- T
  
  source_python("train_rvae_sim.py")
  
  rvae.pred.control <- as.matrix(read_csv(paste0("../results/rvae/",d,"/rvae-",d,"-test.csv"), col_names = FALSE))
  
  return(t(rvae.pred.control))
}
