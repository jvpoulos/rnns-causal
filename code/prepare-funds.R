###################################
# Prepare education spending data for RNNs#
###################################

library(glmnet)

PreProcessData <- function(imp=c('none','linear','locf','median','random','svd'), drop.col){
  # Read data
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  capacity.covariates <- readRDS("data/capacity-covariates.rds")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.covariates$faval[,c("faval.1850","faval.1860")], 
                           capacity.covariates$farmsize[,c("farmsize.1790", "farmsize.1800", "farmsize.1810", "farmsize.1820", "farmsize.1830", "farmsize.1840", "farmsize.1850", "farmsize.1860")],
                           capacity.covariates$access[,c("track2.1835", "track2.1837", "track2.1839", "track2.1840", "track2.1845", "track2.1850", 
                                                         "track2.1851", "track2.1852", "track2.1854", "track2.1856", "track2.1857",
                                                         "track2.1858", "track2.1859", "track2.1860", "track2.1861")])
  
  covars.x <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
  
  # Prepare outcomes data
  educ <- capacity.outcomes$educ.pc
  treat <- educ$mask # NxT masked matrix 
  Y.missing <- educ$M.missing # NxT
  Y <- educ$M # NxT

  Y <- Y[!rownames(Y)%in%drop.col,] #rm 1 from control group for train/test parity
  Y.missing <- educ$M.missing[!rownames(educ$M.missing)%in%drop.col,] # NxT
  covars.x <- covars.x[!rownames(covars.x)%in%drop.col,]
  treat <- treat[!rownames(treat)%in%drop.col,]
  
  if(imp=='none'){
    Y <- Y[, - as.numeric(which(apply(Y, 2, var) == 0))] # rm col from 0 variance
    Y.missing <- Y.missing[,colnames(Y.missing) %in% colnames(Y)]
    treat <- treat[,colnames(treat) %in% colnames(Y)]
  }

  N <- nrow(treat)
  T <- ncol(treat)
  
  treated.indices <- row.names(capacity.outcomes$educ.pc$M)[row.names(capacity.outcomes$educ.pc$M)%in% c("CA", "IA", "KS", "MI", "MN", "MO", "OH", "OR", "WI", "IL", "NV", "AL", "MS", "FL", "LA", "IN")]
  control.indices <-row.names(capacity.outcomes$educ.pc$M)[!row.names(capacity.outcomes$educ.pc$M)%in% c(treated.indices, drop.col)]
  
  t0 <- which(colnames(Y)=="1869")
  
  # Censor post-period treated values
  
  treat_mat <- 1-treat # treated are 0
  Y_obs <- Y * treat_mat
  
  Y_imp <- Y * Y.missing # use for calculating RMSE on non-imputed values
  
  # converting the data to a floating point matrix
  data <- data.matrix(t(Y_obs)) # T x N
  
  # # Splits
  train_data <- data[,colnames(data)%in% control.indices] # train on control units
  
  test_data <- data[,colnames(data)%in% treated.indices]  # treated units
  
  ## Estimate propensity scores
  
  p.mod <- cv.glmnet(x=covars.x, y=(1-treat_mat)[,T], family="binomial")
  W <- predict(p.mod, covars.x, type="response", s = "lambda.min")
  W <- replicate(T,as.vector(W)) # assume constant across T
  
  p.weights <- matrix(NA, nrow=nrow(W), ncol=ncol(W), dimnames = list(rownames(W), colnames(W)))
  p.weights <- (1-treat_mat) + (treat_mat)*W/(1-W) # weighting by the odds
  
  p.weights <- t(p.weights) # T x N
  
  train_w <- p.weights[,colnames(p.weights)%in%colnames(train_data)][rownames(p.weights)%in%rownames(train_data),]
  test_w <- p.weights[,colnames(p.weights)%in%colnames(test_data)][rownames(p.weights)%in%rownames(test_data),]
  
  write.csv(train_data,paste0("data/educ-x-",imp,".csv"),row.names = FALSE)
  write.csv(test_data,paste0("data/educ-y-",imp,".csv"),row.names = FALSE)
  write.csv(train_w,paste0("data/educ-wx-",imp,".csv"),row.names = FALSE)
  write.csv(test_w,paste0("data/educ-wy-",imp,".csv"),row.names = FALSE)
  
  return(list("train_data"=train_data,"test_data"=test_data,"train_w"=train_w,"test_w"=test_w,"t0"=t0,
              "Y"=Y,"Y_imp"=Y_imp,"Y_obs"=Y_obs,"treated.indices"=treated.indices,"p.weights"=p.weights))
}

drop.col <-sample(c("VA","NY","VT","DE","NH","RI","CT","SC","PA","MD","NC","KY","ME","MA","NJ","TN","TX"),1) # randomly drop control unit for treat/control parity

for(imp in c('none','linear','locf','median','random','svd')){
  PreProcessData(imp, drop.col)
}