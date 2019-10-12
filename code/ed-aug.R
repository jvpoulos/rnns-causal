######################################################################
# Experiments comparing ED trained on noisy inputs or with dropout #
######################################################################

library(keras)
library(reticulate)
library(readr)
#use_python("/usr/local/bin/python")
use_python("/usr/bin/python", TRUE)

edAug <- function(Y,treat_indices,d, t0, T, dropout, penalty, GS, GD, multiple, Y2=NULL){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N
  
  # Splits
  train_data <- data[,(-treat_indices)] # train on control units
  
  test_data <- data[,(treat_indices)] # treated units
  
  write.csv(train_data,paste0("../data/",d,"-x.csv"),row.names = FALSE)
  write.csv(test_data,paste0("../data/",d,"-y.csv"),row.names = FALSE)
  
  if(!is.null(Y2)){
    # Converting the data to a floating point matrix
    data2 <- data.matrix(t(Y2)) # T x N
    
    # Splits
    train_data2 <- data2[,(-treat_indices)] # train on control units
    test_data2 <- data2[,(treat_indices)] # treated units
    
    write.csv(train_data2,paste0("../data/",d,"-x2.csv"),row.names = FALSE)
    write.csv(test_data2,paste0("../data/",d,"-y2.csv"),row.names = FALSE)
  }
  
  py <- import_main()
  py$dataname <- d
  py$epochs <- 200
  py$gpu <- 5
  py$t0 <- t0
  py$T <- T
  py$nb_batches <- 4
  py$penalty <- penalty
  py$dropout <- dropout
  py$GS <- GS
  py$GD <- GD
  py$multiple <- multiple
  
  source_python("train_encoder_decoder_aug_sim.py")
  
  ed.pred.control <- as.matrix(read_csv(paste0("../results/encoder-decoder/",d,"/encoder-decoder-",d,"-test.csv"), col_names = FALSE))
  
  return(t(ed.pred.control))
}