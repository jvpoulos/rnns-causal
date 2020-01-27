###################################
# VAR for Synth Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)
library(zoo)

varEst <- function(Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data_truth <- data.matrix(t(Y)) # T x N
  data_ln <- log1p(data_truth) # log transform
  rownames(data_ln) <- 1: nrow(data_ln)
  
  RollingPred<-function(series,treat_indices) {
    # Split and Preprocess
    
    train_data <- series[,(-treat_indices)] # train on control units 
    train_data.tr <- sweep(train_data, 2, train_data[1,]) # subtract values of first t from all values

    test_data <- series[,(treat_indices)] # treated units 
    
    # Fit the model
    var.fit <- lassovar(train_data.tr, lags = 1, horizon = 1)
    # Make predictions
    nextob <- max(as.numeric(rownames(train_data.tr)))+1 # To get the first row that follows the window
    if (nextob<=nrow(data_ln)) {
      preds <- predict(var.fit, data_ln[,(treat_indices)][nextob,] - test_data[1,])
      preds <- sweep(preds, 2, -test_data[1,]) # revert subtraction
      return(preds)
    }
  }    
  
  var.preds <- rollapply(data_ln, width=(t0-1),FUN=RollingPred,by.column=FALSE,align='right',treat_indices=treat_indices)
  var.preds <- expm1(var.preds) # revert log transformation

  return(t(var.preds))
}