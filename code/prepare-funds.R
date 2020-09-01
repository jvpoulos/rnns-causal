###################################
# Prepare education spending data for RNNs#
###################################

library(caret)
library(imputeTS)
library(glmnet)
library(dplyr)

PreProcessData <- function(imp=c('none','linear','locf','median','random','svd')){
  # Read data
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  capacity.covariates <- readRDS("data/capacity-covariates.rds")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.covariates$faval[,c("faval.1850","faval.1860")], 
                           capacity.covariates$farmsize[,c("farmsize.1790", "farmsize.1800", "farmsize.1810", "farmsize.1820", "farmsize.1830", "farmsize.1840", "farmsize.1850", "farmsize.1860")],
                           capacity.covariates$access[,c("track2.1835", "track2.1837", "track2.1839", "track2.1840", "track2.1845", "track2.1850", 
                                                         "track2.1851", "track2.1852", "track2.1854", "track2.1856", "track2.1857",
                                                         "track2.1858", "track2.1859", "track2.1860", "track2.1861")])
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
  
  # Prepare outcomes data
  educ <- capacity.outcomes$educ.pc
  Y.missing <- educ$M.missing # NxT
  Y <- educ$M # NxT 
  Y <- Y[, - as.numeric(which(apply(Y, 2, var) == 0))] # rm col from 0 variance
  Y.missing <- Y.missing[,colnames(Y.missing) %in% colnames(Y)]
  
  treat <- educ$mask # NxT masked matrix 
  treat <- treat[,colnames(treat) %in% colnames(Y)]

  N <- nrow(treat)
  T <- ncol(treat)
  
  treated.indices <- row.names(capacity.outcomes$educ.pc$M)[row.names(capacity.outcomes$educ.pc$M)%in% c("CA", "IA", "KS", "MI", "MN", "MO", "OH", "OR", "WI", "IL", "NV", "AL", "MS", "FL", "LA", "IN")]
  t0 <- ceiling(which(colnames(Y)=="1869")/4) # window size
  
  # Censor post-period treated values
  
  treat_mat <- 1-treat # treated are 0
  Y_obs <- Y * treat_mat
  
  Y_imp <- Y * Y.missing # use for calculating RMSE on non-imputed values
  
  # converting the data to a floating point matrix
  data <- data.matrix(t(Y_obs)) # T x N
  
  # # Splits
  train_data <- data[,!colnames(data)%in% c(treated.indices,"TN")] # train on control units
  
  test_data <- data[,colnames(data)%in% treated.indices]  # treated units
  
  ## Estimate propensity scores
  
  covars.x <- capacity.covars[!rownames(capacity.covars)%in%c(rownames(treated.indices),"TN","GA"),]
  
  capacity.covars.z <- capacity.covars.z
  
  logitMod.x <- cv.glmnet(x=capacity.covars.x, y=as.factor((1-treat_mat)[,t0]), family="binomial", nfolds= nrow(capacity.covars.x), parallel = TRUE, nlambda=400) # LOO
  
  logitMod.z <- cv.glmnet(x=capacity.covars.z, y=as.factor((1-treat_mat)[treated.indices[1],]), family="binomial", nfolds=nrow(capacity.covars.z), parallel = TRUE, nlambda=400)
  
  p.weights.x <- as.vector(predict(logitMod.x, capacity.covars.x, type="response", s ="lambda.min"))
  p.weights.z <- as.vector(predict(logitMod.z, capacity.covars.z, type="response", s ="lambda.min"))
  
  p.weights <- outer(p.weights.x,p.weights.z)   # outer product of fitted values on response scale
  p.weights <- t(p.weights) # T x N
  rownames(p.weights) <-rownames(data)
  colnames(p.weights) <-colnames(data)
  
  train_w <- p.weights[,colnames(p.weights)%in%colnames(train_data)][rownames(p.weights)%in%rownames(train_data),]
  test_w <- p.weights[,colnames(p.weights)%in%colnames(test_data)][rownames(p.weights)%in%rownames(test_data),]
  
  write.csv(train_data,paste0("data/educ-x-",imp,".csv"),row.names = FALSE)
  write.csv(test_data,paste0("data/educ-y-",imp,".csv"),row.names = FALSE)
  write.csv(train_w,paste0("data/educ-wx-",imp,".csv"),row.names = FALSE)
  write.csv(test_w,paste0("data/educ-wy-",imp,".csv"),row.names = FALSE)
  
  return(list("train_data"=train_data,"test_data"=test_data,"train_w"=train_w,"test_w"=test_w,"t0"=t0,
              "Y"=Y,"Y_imp"=Y_imp,"Y_obs"=Y_obs,"treated.indices"=treated.indices,"p.weights"=p.weights))
}

PreProcessData(imp="median")
 
PreProcessData(imp="none")