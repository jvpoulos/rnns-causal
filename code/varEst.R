###################################
# VAR for Synth Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)

varEst <- function(Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data <- data.frame(t(Y)) # T x N
  rownames(data) <- 1: nrow(data)
  
  train_data <- data.frame(data[,(-treat_indices)]) # train on control units 
  
  test_data <- data.frame(data[,(treat_indices)])
  
  # Fit the model
  var.fit <- lassovar(dat=train_data, exo=NULL, lags = 1, horizon = 1, mc=TRUE, ncores=2)
  
  # Fit model on treated units
  
  test_data <- data[,(treat_indices)]
  var.preds <- predict(var.fit, test_data)
  
  return(t(var.preds))
}