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
  
  train_data <- data[,(-treat_indices)] # train on control units
  zero_var_cols <- colnames(train_data[,which(apply(train_data, 2, var) == 0)])
  for(x in zero_var_cols)
  {
    random_values <- sample(train_data[train_data>0],T) # select random non-zero values from training data
    train_data[,x] <- random_values # replace random elements
  }

  print(paste0('zero variance columns: ', sum(apply(train_data, 2, var) == 0)))
  test_data <- data[,(treat_indices)]
  
  # Fit the model
  var.fit <- lassovar(dat=data.frame(train_data), exo=NULL, lags = 1, horizon = 1)
  
  # Fit model on treated units
  
  var.preds <- predict(var.fit, as.matrix(test_data))

  return(t(var.preds)) # N X T
}