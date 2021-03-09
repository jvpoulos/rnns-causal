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

varEst <- function(Y,treat_indices, t0, dfmax=NULL, nlambda=NULL){
  # Converting the data to a floating point matrix
  data <- data.matrix(t(Y)) # T x N
  
  T.max <- nrow(data)
  
  # maxdegrees of freedom ?	
  if(!is.null(dfmax))dfmax<-as.integer(dfmax)	else dfmax	<- NULL
  
  # nlambda ?	
  if(!is.null(nlambda))nlambda<-as.integer(nlambda)	else nlambda	<- NULL
  
  RollingPred<-function(data,treat_indices) {
    # Fit the model
    
    var.fit <- NULL
    try(var.fit <- lassovar(dat=data[,(-treat_indices)][1:(nrow(data)-1),], lags = 1, horizon = 1, dfmax = dfmax, nlambda=nlambda))
    attempt <- 1
    if(is.null(var.fit)){
      while( is.null(var.fit) && attempt <= 25 ) {
        attempt <- attempt + 1
        try(var.fit <- lassovar(dat=perturbR(data[,(-treat_indices)][1:(nrow(data)-1),], prob=0.5), lags = 1, horizon = 1, dfmax = dfmax, nlambda=nlambda))
      }
    }
    
    # var.fit <- try(lassovar(dat=data[,(-treat_indices)], lags = 1, horizon = 1, dfmax = dfmax, nlambda=nlambda))
    # if("try-error" %in% class(var.fit)){
    #   var.fit <- try(lassovar(dat=perturbR(data[,(-treat_indices)], prob=0.5), lags = 1, horizon = 1, dfmax = dfmax, nlambda=nlambda))
    # }
    
    if(nrow(data) < T.max){
      # Make predictions
      preds <- predict(var.fit, data[,(treat_indices)][nrow(data),]) # predict on test data
      return(preds)
    }
  }     
  
  var.preds.test <- rollapply(data=data, width=t0, FUN=RollingPred,by.column=FALSE,align="right",treat_indices=treat_indices)
  
  var.preds <- cbind(data[,(-treat_indices)] , rbind(data[,(treat_indices)][1:(t0-1),], var.preds.test))
  rownames(var.preds) <- rownames(data)
  
  var.preds <- var.preds[,match(colnames(data), colnames(var.preds))] # same order
  
  return(t(var.preds))
}