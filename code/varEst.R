###################################
# VAR for Simulations #
###################################

library(lassovar)
library(zoo)
library(gtools)

perturbR <- function(data, prob){
  ind <- rbinom(length(data), 1, prob) == 1 # elements to be modified with probability 0.5
  data[ind] <- data[ind] + rnorm(sum(ind), 0, 0.1) # adds the perturbation
  return(data)
}

varEst <- function(Y,treat_indices, t0, T){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N
  rownames(data) <- 1: nrow(data)

  RollingPred<-function(series,treat_indices) {
    # Split
    train_data <- series[,(-treat_indices)] # train on control units 

    # Fit the model
    var.fit <- try(lassovar(dat=train_data, lags = 1, horizon = 1))
    if("try-error" %in% class(var.fit)){
      var.fit <- try(lassovar(dat=perturbR(train_data, prob=0.5), lags = 1, horizon = 1))
    }
    if("try-error" %in% class(var.fit)){
      var.fit <- try(lassovar(dat=perturbR(train_data, prob=0.9), lags = 1, horizon = 1))
    }
    
    # Make predictions
    nextob <- max(as.numeric(rownames(train_data)))+1 # To get the first row that follows the window
    
    if (nextob<=nrow(data)) {
      test_data <- data[,(treat_indices)][nextob,]
      preds <- predict(var.fit, test_data)
      return(preds)
    }
  }    
  
  var.preds.test <- rollapply(data, width=(t0-1), FUN=RollingPred,by.column=FALSE,align='right',treat_indices=treat_indices)
  
  var.preds <- cbind(data[,(-treat_indices)] , rbind(data[,(treat_indices)][1:(t0-1),], var.preds.test))
  rownames(var.preds) <- rownames(data)
  
  var.preds <- var.preds[,match(colnames(data), colnames(var.preds))] # same order
  
  return(t(var.preds))
}