###################################
# ED for Synth Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
use_python("/usr/local/bin/python")

ed <- function(Y_obs,Y,treat_indices,d, t0, T){
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
  if(d=='stock'){
    py$nb_batches <- 32
    py$gpu <- 0
    py$epochs <- 10000
    py$lr <- 0.0005
    py$penalty <- 0.001
    py$dr <- 0.2
  } 
  if(d=='stock_fixed'){
    py$nb_batches <- 32
    py$gpu <- 1
    py$epochs <- 10000
    py$lr <- 0.0005
    py$penalty <- 0.001
    py$dr <- 0.2
  } 
  if(d=='educ.pc'){
    py$nb_batches <- 8
    py$gpu <- 0
    py$epochs <- 10000
    py$lr <- 0.0005
    py$penalty <- 0.001
    py$dr <- 0.2
  } 
  if(d=='votediff'){
    py$nb_batches <- 8
    py$gpu <- 1
    py$epochs <- 10000
    py$lr <- 0.0005
    py$penalty <- 0.001
    py$dr <- 0.2
  } 
  else{
    py$nb_batches <- 4
    py$gpu <- 0
    py$epochs <- 10000
    py$lr <- 0.0005
    py$penalty <- 0.001
    py$dr <- 0.2
  }
  
  source_python("code/train_encoder_decoder_sim.py")
  
  ed.pred <- as.matrix(read_csv(paste0("results/encoder-decoder/",d,"/encoder-decoder-",d,"-test.csv"), col_names = FALSE))
  
  return(t(ed.pred))
}