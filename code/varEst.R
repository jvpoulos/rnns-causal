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
  
  train_data <- data[,(-treat_indices)]
  train_data_scaled <- scale(train_data) # https://stackoverflow.com/questions/49260862/trainable-sklearn-standardscaler-for-r
  
  test_data <- data[,(treat_indices)]
  test_data_scaled <- scale(test_data, center=attr(train_data_scaled, "scaled:center"), 
                        scale=attr(train_data_scaled, "scaled:scale"))

  # Fit the model
  var.fit <- lassovar(dat=data.frame(train_data_scaled), exo=NULL, lags = 1, horizon = 1)
  
  # Fit model on treated units
  
  var.preds <- predict(var.fit, as.matrix(test_data_scaled))

  return(t(var.preds)) # N X T
}