###################################
# LSTM for Simulations #
###################################

library(keras)
library(reticulate)
library(readr)
# use_python("/usr/local/bin/python")
#use_python("~/venv/bin/python") # comet
use_python("/usr/bin/python3") # dcc

lstm <- function(Y,p.weights,treat_indices,d, t0, T, config=NULL){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N
  data_w <- data.matrix(t(p.weights)) # T x N

  # Splits
  train_data <- data[,(-treat_indices)] # train on control units
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
  py$n_hidden <- 128
  py$activation <- 'tanh'
  if(d%in%c('educ','educ.pc','educ_benchmark','educ_placebo_pre','educ_rnns')){
    py$lr <- 0.001
    py$dr <- 0.5
    py$penalty <- 0.01
    py$nb_batches <- 32
    py$patience <- 25
  }
  if(d%in%c('stock','stock_plot','rbf','sine')){
    py$lr <- 0.001
    py$penalty <- 0.01
    py$patience <- 25
    py$nb_batches <- 32
    py$dr <- 0.5
  }
  if(!is.null(config)){
    print(config)
    py$activation <- as.character(config[[1]])
    py$n_hidden <- as.numeric(config[2])
    py$patience <- as.numeric(config[3])
    py$dr <- as.numeric(config[4])
  }
  
  source_python("code/train_lstm_sim.py")
  
  lstm.pred.test <- as.matrix(read_csv(paste0("results/lstm/",d,"/lstm-",d,"-test.csv"), col_names = FALSE))
  colnames(lstm.pred.test) <- colnames(test_data)
  
  lstm.pred <- cbind(train_data, rbind(test_data[1:t0,], lstm.pred.test))
  rownames(lstm.pred) <- rownames(test_data)
  
  lstm.pred <-lstm.pred[,match(colnames(data), colnames(lstm.pred))] # same order
  
  return(t(lstm.pred))  # N x T
}
