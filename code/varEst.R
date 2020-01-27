###################################
# VAR for Synth Simulations #
###################################

#library(devtools)
#install_github("lcallot/lassovar")
library(lassovar)
library(zoo)
library(caret)

varEst <- function(Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N
  rownames(data) <- 1: nrow(data)
  
  RollingPred<-function(series,treat_indices) {
    # Split
    train_data <- series[,(-treat_indices)] # train on control units 

    # Fit the model
    var.fit <- lassovar(dat=train_data, exo=NULL, lags = 1, horizon = 1, mc=TRUE, ncores=2)
    
    # Make predictions
    nextob <- max(as.numeric(rownames(train_data)))+1 # To get the first row that follows the window
    
    if (nextob<=nrow(data)) {
      test_data <- data[,(treat_indices)][nextob,]
      preds <- predict(var.fit, test_data)
      return(preds)
    }
  }    
  
  var.preds <- rollapply(data, width=(t0-1),FUN=RollingPred,by.column=FALSE,align='right',treat_indices=treat_indices)

  return(t(var.preds))
}