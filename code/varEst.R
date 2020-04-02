###################################
# VAR for Synth Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)
library(gtools)

varEst <- function(Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data <- data.frame(t(Y)) # T x N
  rownames(data) <- 1: nrow(data)
  
  train_data <- data[,(-treat_indices)] # train on control units
  test_data <- data[,(treat_indices)]
  
  # Remove columns with zero variance
  if(sum(apply(train_data, 2, var) == 0)>0){
    train_data_new <- train_data[ - as.numeric(which(apply(train_data, 2, var) == 0))]
    test_data_new <- test_data[ - as.numeric(which(apply(train_data, 2, var) == 0))] 
  } else{
    train_data_new <- train_data
    test_data_new <- test_data
  }

  # Fit the model
  var.fit <- lassovar(dat=data.frame(train_data_new), exo=NULL, lags = 1, horizon = 1)
  
  # Fit model on treated units
  
  var.preds <- predict(var.fit, as.matrix(test_data_new))
  
  if(sum(apply(train_data, 2, var) == 0)>0){ # NAs for columns removed
    removed_columns <- matrix(NA, nrow(test_data_new), length( as.numeric(which(apply(train_data, 2, var) == 0))))
    colnames(removed_columns) <- setdiff(colnames(test_data),colnames(test_data_new))
    var.preds <- cbind(var.preds, removed_columns)
    var.preds <- var.preds[,mixedsort(colnames(var.preds))]
  }

  return(t(var.preds)) # N X T
}