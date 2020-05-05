###################################
# VAR for Synth Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)
library(gtools)

varEst <- function(Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # (T x N)
  
  train_data <- diff(log(data[,(-treat_indices)])) # first difference for linear trend and log for non-linear trend (T x N)
  test_data <- diff(log(data[,(treat_indices)]))
  
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
  
  var.preds <-   sapply(1:ncol(var.preds), function(i){
      exp(cumsum(c(data[,(treat_indices)][1,i],var.preds[,i])) + log(data[,(treat_indices)][1,i]))
  }) # revert transformations

  return(t(var.preds)) # N X T
}