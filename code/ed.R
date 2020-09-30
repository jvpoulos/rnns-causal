###################################
# ED for Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
# use_python("/usr/local/bin/python")
#use_python("~/venv/bin/python") # comet
use_python("/usr/bin/python3") # dcc

ed <- function(Y,p.weights,treat_indices,d, t0, T, config=NULL){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N
  data_w <- data.matrix(t(p.weights)) # T x N

  # Splits
  train_data <- data[,(-treat_indices)] # control units
  train_w <- data_w[,(-treat_indices)]
  
  test_data <- data[,(treat_indices)] # treated units
  test_w <- data_w[,(treat_indices)]

  write.csv(train_data,paste0("data/",d,"-x.csv"),row.names = FALSE)
  write.csv(test_data,paste0("data/",d,"-y.csv"),row.names = FALSE)

  write.csv(train_w,paste0("data/",d,"-wx.csv"),row.names = FALSE)
  write.csv(test_w,paste0("data/",d,"-wy.csv"),row.names = FALSE)
  
  py <- import_main()
  py$dataname <- d
  py$t0 <- t0
  py$T <- T
  py$gpu <- 3
  py$epochs <- 500
  py$lr <- 0.001
  py$dr <- 0.2
  py$penalty <- 0.001
  py$nb_batches <- 32
  py$encoder_hidden_1 <- 128
  py$encoder_hidden_2 <- 128
  py$decoder_hidden <- 128
  py$patience <- 25
  py$activation <- 'tanh'
  if(d%in%c('stock','stock-plot','rbf','sine')){
    py$nb_batches <- 128
    py$patience <- 10
    py$dr <- 0.5
  }
  if(!is.null(config)){
    print(config)
    py$activation <- as.character(config[[1]])
    py$encoder_hidden_1 <-  as.numeric(config[2])
    py$encoder_hidden_2 <-  as.numeric(config[2])
    py$decoder_hidden <-  as.numeric(config[2])
    py$patience <- as.numeric(config[3])
    py$dr <- as.numeric(config[4])
  }
  
  source_python("code/train_encoder_decoder_sim.py")
  
  ed.pred.test <- as.matrix(read_csv(paste0("results/encoder-decoder/",d,"/encoder-decoder-",d,"-test.csv"), col_names = FALSE))
  colnames(ed.pred.test) <- colnames(test_data)
  
  ed.pred <- cbind(train_data, rbind(test_data[1:t0,], ed.pred.test))
  rownames(ed.pred) <- rownames(test_data)
  
  ed.pred <- ed.pred[,match(colnames(data), colnames(ed.pred))] # same order
  
  return(t(ed.pred)) # N x T
}