######################################################################
# Education Spending Application: Comparison estimators #
######################################################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(dplyr)
library(glmnet)
library(caret)
library(imputeTS)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

source("code/utils.R")

CapacityCompare <- function(imp, run.CI=TRUE){
  
  # Read data
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  
  # Prepare outcomes data
  educ <- capacity.outcomes$educ.pc
  Y <- educ$M # NxT 
  Y <- Y[!rownames(Y)%in%c("TN"),] #rm TN from control group for train/test parity
  
  treat <- educ$mask[!rownames(educ$mask)%in%c("TN"),] # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  treated.indices <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
  t0 <- which(colnames(Y)=="1869") # first treatment time # same for all outcomes
  
  treat[rownames(treat)%in% treated.indices,][,as.numeric(colnames(treat)) >= 1869] <- 1# adjust for simultaneous adoption 
  
  # Censor post-period treated values
  
  treat_mat <- 1-treat
  Y_obs <- Y*treat_mat

  ## Compare different methods
  
  ## -----
  ## ADH
  ## -----
  est_model_ADH <- adh_mp_rows(Y_obs, treat_mat)
  est_model_ADH_test_att <- (1/sum(1-treat_mat)) * sum((Y-est_model_ADH)*(1-treat_mat))
  est_model_ADH_test_att
  
  # t.stat.adh <- rowMeans(t(Y[!rownames(Y)%in%treated.indices,][,t0:T])- t(est_model_ADH[!rownames(est_model_ADH)%in%treated.indices,][,t0:T]))
  # 
  # if(run.CI){
  #   estimator <- "adh"
  #   CI.treated <- PermutationCI(forecast=t(est_model_ADH[!rownames(est_model_ADH)%in%treated.indices,][,t0:T]), 
  #                               true=t(Y[!rownames(Y)%in%treated.indices,][,t0:T]), 
  #                               t.stat=est_model_ADH_test_att,
  #                               n.placebo=(nrow(Y[!rownames(Y)%in%treated.indices,])-1), 
  #                               np=10000, 
  #                               l=10, 
  #                               prec=1e-03)
  #   
  #   saveRDS(CI.treated, paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,".rds"))
  # } else{
  #   CI.treated <- readRDS(paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,".rds"))
  # }

  # ## ------
  # ## RVAE
  # ## ------
  # 
  # est_model_RVAE <-  read.csv(paste0("results/rvae/educ/rvae-educ-test-",imp,".csv"), header=FALSE)
  # est_model_RVAE_test_att <- (1/sum(1-treat_mat)) * sum(Y[rownames(Y) %in% treated.indices,][,t0:T] -est_model_RVAE)
  # est_model_RVAE_test_att
  # 
  # ## ------
  # ## ED
  # ## ------
  # 
  # est_model_ED <- read.csv(paste0("results/encoder-decoder/educ/encoder-decoder-educ-test-",imp,".csv"), header=FALSE)
  # est_model_ED_test_att <- (1/sum(1-treat_mat)) * sum(Y[rownames(Y) %in% treated.indices,][,t0:T] -est_model_ED)
  # est_model_ED_test_att
  # 
  # ## ------
  # ## LSTM
  # ## ------
  # 
  # est_model_LSTM <- read.csv(paste0("results/lstm/educ/lstm-educ-test-",imp,".csv"), header=FALSE)
  # est_model_LSTM_test_att <- (1/sum(1-treat_mat)) * sum(Y[rownames(Y) %in% treated.indices,][,t0:T] -est_model_LSTM)
  # est_model_LSTM_test_att

  ## ------
  ## VAR
  ## ------
  
  source("code/varEst.R")
  est_model_VAR <- varEst(Y, treat_indices=c(which(rownames(Y) %in% treated.indices)), t0, T)
  est_model_VAR_test_att <-  (1/sum(1-treat_mat)) * sum(Y[rownames(Y) %in% treated.indices,][,t0:T] - est_model_VAR[,t0:T]) 
  est_model_VAR_test_att

  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 3)
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  est_model_MCPanel$test_att <- (1/sum(1-treat_mat)) * sum((Y-est_model_MCPanel$Mhat)*(1-treat_mat))
  est_model_MCPanel$test_att
  
  ## -----
  ## DID
  ## -----
  
  est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
  est_model_DID_test_att <- (1/sum(1-treat_mat)) * sum((Y-est_model_DID)*(1-treat_mat))
  est_model_DID_test_att

}

CapacityCompare(imp="locf")