###################################
# VAR for Synth Simulations #
###################################

library(onlineVAR)

varEst <- function(Y_obs,Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data_obs <- data.matrix(t(Y_obs)) # T x N
  data_truth <- data.matrix(t(Y)) # T x N
  
  # Splits
  train_data <- data_obs[,(-treat_indices)] # train on control units
  
  test_data <- data_truth[,(treat_indices)] # treated units
  
  var.fit <- onlineVAR(train_data, nu = 0.99, lags = 1, ahead = 1)

  var.pred.test <- predict(var.fit, newdata = test_data)[(t0:T),]
    
  return(t(var.pred.test))
}
