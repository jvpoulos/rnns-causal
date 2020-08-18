###################################
# VAR for Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)
library(gtools)

varEst <- function(Y, treat_indices, scale=TRUE, cores){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # (T x N)
  
  train_data <- data[,(-treat_indices)]
  test_data <- data[,(treat_indices)]
  
  # Fit the model
  var.fit <- lassovar(dat=data.frame(train_data), exo=NULL, lags = 1, horizon = 1, mc=TRUE, ncores=cores) # standardize by default
  
  # Fit model on treated units
  
  var.preds <- predict(var.fit, as.matrix(test_data))
  colnames(var.preds) <- colnames(test_data)
  
  var.preds <- cbind(train_data, var.preds)

  var.preds <-var.preds[,match(colnames(data), colnames(var.preds))] # same order

  return(t(var.preds)) # N X T
}