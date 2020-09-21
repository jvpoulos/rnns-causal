###################################
# Prepare education spending data for RNNs#
###################################

library(glmnet)

PreProcessData <- function(imp=c("none","locf","linear","random","mean","ma")){
  # Read data
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  capacity.covariates <- readRDS("data/capacity-covariates.rds")
  
  # Prepare outcomes data
  educ <- capacity.outcomes$educ.pc
  treat <- educ$mask # NxT masked matrix 
  Y.missing <- educ$M.missing # NxT
  Y <- educ$M # NxT
  
  if(imp=='none'){
    Y <- Y[, - as.numeric(which(apply(Y, 2, var) == 0))] # rm cols with 0 variance
    Y.missing <- Y.missing[,colnames(Y.missing) %in% colnames(Y)]
    treat <- treat[,colnames(treat) %in% colnames(Y)]
  }
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  
  treated.indices <- row.names(capacity.outcomes$educ.pc$M)[row.names(capacity.outcomes$educ.pc$M)%in% pub.states]

  t0 <- which(colnames(Y)=="1869")
  
  # Transform covars to unit and time-specific inputs
  common_rows <- intersect(intersect(rownames(capacity.covariates$faval), rownames(capacity.covariates$farmsize)), rownames(capacity.covariates$access))
  capacity.covars <- cbind(capacity.covariates$faval[,c("faval.1850","faval.1860")][common_rows,], 
                           capacity.covariates$farmsize[,c("farmsize.1790", "farmsize.1800", "farmsize.1810", "farmsize.1820", "farmsize.1830", "farmsize.1840", "farmsize.1850", "farmsize.1860")][common_rows,],
                           capacity.covariates$access[,-c(1)][common_rows,])
  
  covars.x <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
  covars.x[is.na(covars.x)] <- 0
  
  # Censor post-period treated values
  
  treat_mat <- 1-treat # treated are 0
  Y_obs <- Y * treat_mat
  
  Y_imp <- Y * Y.missing # use for calculating RMSE on non-imputed values
  
  # converting the data to a floating point matrix
  data <- data.matrix(t(Y_obs)) # T x N
  
  # # Splits
  train_data <- data[,!colnames(data)%in% treated.indices] # train on control units
  
  test_data <- data[,colnames(data)%in% treated.indices]  # treated units
  test_data <- test_data[,! colnames(test_data) %in% setdiff(colnames(test_data),rownames(covars.x))] # drop treated without covariates
 
  set.seed(1280)
  test_data <- test_data[, sample(1:ncol(test_data), ncol(train_data))] # dimensional parity with train set
  treated.indices <- colnames(test_data)
  
  treat_mat <- treat_mat[rownames(treat_mat) %in% c(colnames(train_data),colnames(test_data)),]
  covars.x <- covars.x[rownames(covars.x) %in% c(colnames(train_data),colnames(test_data)),]
  Y_obs <- Y_obs[rownames(Y_obs) %in% c(colnames(train_data),colnames(test_data)),]
  treat <- treat[rownames(treat) %in% c(colnames(train_data),colnames(test_data)),]
  
  ## Estimate propensity scores
  
  p.mod <- cv.glmnet(x=cbind(covars.x,Y_obs[,1:(t0-1)]), y=(1-treat_mat), family="mgaussian")
  W <- predict(p.mod, cbind(covars.x,Y_obs[,1:(t0-1)]), s = "lambda.min")[,,1]
  W[,1:(t0-1)] <- W[,t0] # assume pre-treatment W same as t0
  W[W <=0.01] <- 0.01 # threshold values
  W[W >=1] <- 1-0.01 # threshold values
  
  p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
  p.weights <- treat*(1-W) + (1-treat)*(W)
  
  p.weights <- t(p.weights) # T x N
  
  train_w <- p.weights[,colnames(p.weights)%in%colnames(train_data)][rownames(p.weights)%in%rownames(train_data),]
  test_w <- p.weights[,colnames(p.weights)%in%colnames(test_data)][rownames(p.weights)%in%rownames(test_data),]
  
  print("writing to csv")
  write.csv(train_data,paste0("data/educ-x-",imp,".csv"),row.names = FALSE)
  write.csv(test_data,paste0("data/educ-y-",imp,".csv"),row.names = FALSE)
  write.csv(train_w,paste0("data/educ-wx-",imp,".csv"),row.names = FALSE)
  write.csv(test_w,paste0("data/educ-wy-",imp,".csv"),row.names = FALSE)
  
  return(list("train_data"=train_data,"test_data"=test_data,"train_w"=train_w,"test_w"=test_w,"t0"=t0,
              "Y"=Y,"Y_imp"=Y_imp,"Y_obs"=Y_obs,"treated.indices"=treated.indices,"p.weights"=p.weights))
}

for(imp in c("none","locf","linear","random","mean","ma")){
  print(imp)
  PreProcessData(imp)
}