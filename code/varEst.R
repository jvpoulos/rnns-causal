###################################
# VAR for Synth Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)

varEst <- function(Y_obs,Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data_obs <- data.matrix(t(Y_obs)) # T x N
  data_truth <- data.matrix(t(Y)) # T x N
  
  # Splits
  train_data <- log1p(data_obs[,(-treat_indices)]) # train on control units # log transform
  
  test_data <- log1p(data_truth[,(treat_indices)]) # treated units # log transform
  
  var.fit <- lassovar(ts(train_data), lags = 1, horizon = 1)
  var.preds <- expm1(predict(var.fit, ts(test_data))) # revert log transformation

  return(t(var.preds))
}
