######################################################################
# Education spendning placebo estimates on pretreatment periods  #
######################################################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(softImpute)
library(readr)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

source("code/permutationTest.R")

CapacitySim <- function(outcomes,covars.x,d,N,sim,imp){
  
  ## Analysis 1: ST vs AT (retrospective, X=CBW) 
  
  Y <- outcomes[[d]]$M # NxT 
  Y.missing <- outcomes[[d]]$M.missing # NxT 
  treat <- outcomes[[d]]$mask # NxT masked matrix 
  
  train_data <- read_csv(paste0("data/educ-x-",imp,".csv"), col_names = TRUE)
  test_data <- read_csv(paste0("data/educ-y-",imp,".csv"), col_names = TRUE)
  
  treated.indices <- colnames(test_data)
  control.indices <- colnames(train_data)
  
  # Use pre-treatment (all zeros)
  t0 <- which(colnames(Y)=="1869") # real t0
  
  treat <- treat[,1:(t0-1)]
  Y <- Y[,1:(t0-1)]
  Y.missing <- Y.missing[,1:(t0-1)]
  
  # Parity
  treat <- treat[rownames(treat)%in%c(colnames(train_data),colnames(test_data)),]
  Y.missing <- Y.missing[rownames(Y.missing)%in%c(colnames(train_data),colnames(test_data)),]
  Y <- Y[rownames(Y)%in%c(colnames(train_data),colnames(test_data)),]
  capacity.covars <- capacity.covars[rownames(capacity.covars)%in%c(colnames(train_data),colnames(test_data)),]
  
  T <- ncol(Y)
  N <- nrow(Y)
  
  # Random staggered adoption among actual treated 
  t0 <- round(ncol(treat)/2) # placebo t0
  treat_indices <- which(rownames(Y)%in%treated.indices) # keep treated fixed to actual treated
  control_indices <- which(!rownames(Y)%in%treated.indices)
  if(sim==1){
    treat_mat <- stag_adapt(Y, length(treat_indices),t0, treat_indices)
  } else{
    treat_mat <- simul_adapt(Y, length(treat_indices),t0, treat_indices) 
  }
  
  rownames(treat_mat) <- rownames(Y)
  colnames(treat_mat) <- colnames(Y)
  
  Y_obs <- Y * treat_mat # treated are 0
  
  # For randomization CIs
  treat_mat_c <- simul_adapt(Y, N_t = length(control_indices), T0=t0, treat_indices = control_indices)
  Y_obs_c <- Y * treat_mat_c
  
  ## Estimate propensity scores
  
  p.mod <- cv.glmnet(x=cbind(capacity.covars,Y_obs[,1:(t0-1)]), y=(1-treat_mat), family="mgaussian")
  W <- predict(p.mod, cbind(capacity.covars,Y_obs[,1:(t0-1)]), s = "lambda.min")[,,1]
  W[,1:(t0-1)] <- W[,t0] # assume pre-treatment W same as t0
  W[W <=0.01] <- 0.01 # threshold values
  W[W >=1] <- 1-0.01 # threshold values
  
  p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
  p.weights <- treat*(1-W) + (1-treat)*(W)
  
  ## ------
  ## LSTM
  ## ------
  
  source("code/lstm.R")
  est_model_LSTM <- lstm(Y_obs, p.weights, treat_indices=which(rownames(Y_obs) %in% treated.indices), d='educ_placebo_pre', t0=20, T)
  if(imp=='none'){
    LSTM_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_LSTM[rownames(est_model_LSTM)%in%treated.indices,][,t0:T], na.rm=TRUE)
    LSTM_ATT <- LSTM_ATT[LSTM_ATT!="NaN"]
  }else{
    LSTM_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_LSTM[rownames(est_model_LSTM)%in%treated.indices,][,t0:T])
  }
  
  est_model_LSTM_c <- lstm(Y_obs_c, p.weights, treat_indices=which(rownames(Y_obs) %in% control.indices), d='educ_placebo_pre', t0=20, T) 
  
  LSTM_CI_treated <- PermutationCI(forecast=t(est_model_LSTM_c[rownames(est_model_LSTM_c)%in%control.indices,][,t0:T]), 
                                   true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                   t.stat=LSTM_ATT,
                                   n.placebo=ncol(train_data)-1, 
                                   np=10000, 
                                   l=500, 
                                   prec=1e-03)
  
  ## ------
  ## ED
  ## ------
  
  source("code/ed.R")
  est_model_ED <- ed(Y_obs, p.weights, treat_indices=which(rownames(Y_obs) %in% treated.indices), d='educ_placebo_pre', t0=20, T)
  if(imp=='none'){
    ED_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_ED[rownames(est_model_ED)%in%treated.indices,][,t0:T], na.rm=TRUE)
    ED_ATT <- ED_ATT[ED_ATT!="NaN"]
  }else{
    ED_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_ED[rownames(est_model_ED)%in%treated.indices,][,t0:T])
  }
  
  est_model_ED_c <- lstm(Y_obs_c, p.weights, treat_indices=which(rownames(Y_obs) %in% control.indices), d='educ_placebo_pre', t0=20, T) 
  
  ED_CI_treated <- PermutationCI(forecast=t(est_model_ED_c[rownames(est_model_ED_c)%in%control.indices,][,t0:T]), 
                                 true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                 t.stat=ED_ATT,
                                 n.placebo=ncol(train_data)-1, 
                                 np=10000, 
                                 l=500, 
                                 prec=1e-03)
  
  ## -----
  ## ADH
  ## -----
  print("ADH Started")
  source("code/ADH.R") # clip gradients
  est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter = 200, rel_tol = 1e-05)
  if(imp=='none'){
    ADH_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_ADH[rownames(est_model_ADH)%in%treated.indices,][,t0:T], na.rm=TRUE)
    ADH_ATT <- ADH_ATT[ADH_ATT!="NaN"]
  }else{
    ADH_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_ADH[rownames(est_model_ADH)%in%treated.indices,][,t0:T])
  }
  
  est_model_ADH_c <- adh_mp_rows(Y_obs_c, treat_mat_c, niter = 200, rel_tol = 1e-05) 
  
  ADH_CI_treated <- PermutationCI(forecast=t(est_model_ADH_c[rownames(est_model_ADH_c)%in%control.indices,][,t0:T]), 
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                  t.stat=ADH_ATT,
                                  n.placebo=ncol(train_data)-1, 
                                  np=10000, 
                                  l=500, 
                                  prec=1e-03)
  # ## ------
  # ## VAR
  # ## ------
  # 
  # source("code/varEst.R")
  # est_model_VAR <- varEst(Y_obs, treat_indices=treated.indices)
  # if(imp=='none'){
  #   VAR_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_VAR[rownames(est_model_VAR)%in%treated.indices,][,t0:T], na.rm=TRUE)
  #   VAR_ATT <- VAR_ATT[VAR_ATT!="NaN"]
  # }else{
  #   VAR_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_VAR[rownames(est_model_VAR)%in%treated.indices,][,t0:T])
  # }
  # 
  # est_model_VAR_c <- varEst(Y_obs_c, treat_indices=control.indices) 
  # 
  # VAR_CI_treated <- PermutationCI(forecast=t(est_model_VAR_c[rownames(est_model_VAR_c)%in%control.indices,][,t0:T]), 
  #                                 true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
  #                                 t.stat=VAR_ATT,
  #                                 n.placebo=ncol(train_data)-1, 
  #                                 np=10000, 
  #                                 l=500, 
  #                                 prec=1e-03)
  # 
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm(Y_obs, treat_mat, W=p.weights, to_estimate_u = 1, to_estimate_v = 1, lambda_L = c(0.05), niter = 200, rel_tol = 1e-05)[[1]] # no CV to save computational time
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  rownames(est_model_MCPanel$Mhat) <- rownames(Y)
  colnames(est_model_MCPanel$Mhat) <- colnames(Y)
  
  if(imp=='none'){
    MCPanel_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_MCPanel$Mhat[rownames(est_model_MCPanel$Mhat)%in%treated.indices,][,t0:T], na.rm=TRUE)
    MCPanel_ATT <- MCPanel_ATT[MCPanel_ATT!="NaN"]
  }else{
    MCPanel_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_MCPanel$Mhat[rownames(est_model_MCPanel$Mhat)%in%treated.indices,][,t0:T])
  }
  
  est_model_MCPanel_c <- mcnnm(Y_obs_c, treat_mat_c, W=p.weights, to_estimate_u = 1, to_estimate_v = 1, lambda_L = c(0.05), niter = 200, rel_tol = 1e-05)[[1]] # no CV to save computational time
  est_model_MCPanel_c$Mhat <- est_model_MCPanel_c$L + replicate(T,est_model_MCPanel_c$u) + t(replicate(N,est_model_MCPanel_c$v))
  rownames(est_model_MCPanel_c$Mhat) <- rownames(treat)
  MCPanel_CI_treated <- PermutationCI(forecast=t(est_model_MCPanel_c$Mhat[rownames(est_model_MCPanel_c$Mhat)%in%control.indices,][,t0:T]), 
                                      true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                      t.stat=MCPanel_ATT,
                                      n.placebo=ncol(train_data)-1, 
                                      np=10000, 
                                      l=500, 
                                      prec=1e-03)
  
  ## -----
  ## DID
  ## -----
  
  est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
  if(imp=='none'){
    DID_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_DID[rownames(est_model_DID)%in%treated.indices,][,t0:T], na.rm=TRUE)
    DID_ATT <- DID_ATT[DID_ATT!="NaN"]
  }else{
    DID_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_DID[rownames(est_model_DID)%in%treated.indices,][,t0:T])
  }
  
  est_model_DID_c <- t(DID(t(Y_obs_c), t(treat_mat_c)))
  DID_CI_treated <- PermutationCI(forecast=t(est_model_DID_c[rownames(est_model_DID_c)%in%control.indices,][,t0:T]), 
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                  t.stat=DID_ATT,
                                  n.placebo=ncol(train_data)-1, 
                                  np=10000, 
                                  l=500, 
                                  prec=1e-03)
  
  ## -----
  ## VT-EN: : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1, num_lam = 5, num_folds = nrow(t(Y_obs)))) # avoid constant y
  if(imp=='none'){
    ENT_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T] - est_model_ENT[rownames(est_model_ENT)%in%treated.indices,][,t0:T], na.rm=TRUE)
    ENT_ATT <- ENT_ATT[ENT_ATT!="NaN"]
  }else{
    ENT_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T] - est_model_ENT[rownames(est_model_ENT)%in%treated.indices,][,t0:T])
  }
  
  est_model_ENT_c <- t(en_mp_rows(t(Y_obs_c), t(treat_mat_c), num_alpha = 1, num_lam = 5, num_folds = nrow(t(Y_obs_c)))) # avoid constant y
  ENT_CI_treated <- PermutationCI(forecast=t(est_model_ENT_c[rownames(est_model_ENT_c)%in%control.indices,][,t0:T]), 
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                  t.stat=ENT_ATT,
                                  n.placebo=ncol(train_data)-1, 
                                  np=10000, 
                                  l=500, 
                                  prec=1e-03)
  
  ## Saving data
  
  df1 <-
    data.frame(
      "y" =  c(DID_ATT,ED_ATT,LSTM_ATT,MCPanel_ATT,ADH_ATT,ENT_ATT),
      "CI.lower" = c(DID_CI_treated[,1],ED_CI_treated[,1],LSTM_CI_treated[,1],MCPanel_CI_treated[,1],ADH_CI_treated[,1],ENT_CI_treated[,1]),
      "CI.upper" = c(DID_CI_treated[,2],ED_CI_treated[,2],LSTM_CI_treated[,2],MCPanel_CI_treated[,2],ADH_CI_treated[,2],ENT_CI_treated[,2]),
      "Method" = c("DID", 
                   "Encoder-decoder",
                   "LSTM",
                   "MC-NNM", 
                   "SCM",
                   "SCM-L1"))
  
  filename <- paste0("educ-placebo-pretreatment-",imp,sim,".rds")
  save(df1, file = paste0("results/",filename))
}

# Read data
capacity.outcomes <- readRDS("data/capacity-outcomes-locf.rds")
capacity.covariates <- readRDS("data/capacity-covariates.rds")

print(dim(capacity.outcomes$educ.pc$M))

# Transform covars to unit and time-specific inputs
common_rows <- intersect(intersect(rownames(capacity.covariates$faval), rownames(capacity.covariates$farmsize)), rownames(capacity.covariates$access))
capacity.covars <- cbind(capacity.covariates$faval[,c("faval.1850","faval.1860")][common_rows,], 
                         capacity.covariates$farmsize[,c("farmsize.1790", "farmsize.1800", "farmsize.1810", "farmsize.1820", "farmsize.1830", "farmsize.1840", "farmsize.1850", "farmsize.1860")][common_rows,],
                         capacity.covariates$access[,-c(1)][common_rows,])

capacity.covars <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
capacity.covars[is.na(capacity.covars)] <- 0

for(sim in c(0,1)){
  CapacitySim(outcomes=capacity.outcomes,covars.x=capacity.covars, d="educ.pc", sim=sim, imp="locf")
}