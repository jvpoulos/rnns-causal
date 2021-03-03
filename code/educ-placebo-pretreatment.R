######################################################################
# Education spending placebo estimates on pretreatment periods  #
######################################################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(readr)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

source("code/permutationTest.R")

CapacitySim <- function(outcomes,covars.x,d,sim,imp,if.save=TRUE){
  
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
  
  T <- ncol(Y)
  N <- nrow(Y)
  
  # Random staggered adoption among actual treated 
  t0.placebo <- round(c(ncol(treat)*(2/3)))
  treat_indices <- which(rownames(Y)%in%treated.indices) # keep treated fixed to actual treated
  control_indices <- which(!rownames(Y)%in%treated.indices)
  if(sim==1){
    treat_mat <- stag_adapt(Y, length(treat_indices),t0.placebo, treat_indices)
  } else{
    treat_mat <- simul_adapt(Y, length(treat_indices),t0.placebo, treat_indices) 
  }
  
  rownames(treat_mat) <- rownames(Y)
  colnames(treat_mat) <- colnames(Y)
  
  Y_obs <- Y * treat_mat # treated are 0
  
  # For randomization CIs
  treat_mat_c <- stag_adapt(Y, N_t = length(control_indices), T0=t0.placebo, treat_indices = control_indices)
  Y_obs_c <- Y * treat_mat_c
  
  ## Estimate propensity scores
  
  p.mod <- cv.glmnet(x=cbind(capacity.covars,Y[,1:(t0.placebo-1)]), y=(1-treat_mat), family="mgaussian")
  W <- predict(p.mod, cbind(capacity.covars,Y[,1:(t0.placebo-1)]), s = "lambda.min")[,,1]
  W[,1:(t0.placebo-1)] <- W[,t0.placebo] # assume pre-treatment W same as t0.placebo
  
  if(min(W)<0 | max(W>=1)){ # threshold values
    W[W <=0 ] <- min(W[W>0]) # replace with min. pos value
    W[W >=1] <- 1-min(W[W>0]) 
  }
  
  p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
  p.weights <- treat*(1-W) + (1-treat)*(W)
  
  ## ------
  ## VAR
  ## ------
  
  source("code/varEst.R")
  est_model_VAR <- varEst(Y, treat_indices=treated.indices, t0.placebo, T)
  if(imp=='none'){
    VAR_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_VAR[rownames(est_model_VAR)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    VAR_ATT <- VAR_ATT[VAR_ATT!="NaN"]
  }else{
    VAR_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_VAR[rownames(est_model_VAR)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_VAR_c <- varEst(Y, treat_indices=control.indices, t0.placebo, T)
  
  VAR_CI_treated <- PermutationCI(forecast=t(est_model_VAR_c[rownames(est_model_VAR_c)%in%control.indices,][,t0.placebo:T]),
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]),
                                  t.stat=VAR_ATT,
                                  n.placebo=ncol(train_data)-1,
                                  np=10000,
                                  l=5000,
                                  prec=1e-03)
  
  VAR_CI_treated[is.infinite(VAR_CI_treated)] <- NA
  
  ## ------
  ## LSTM
  ## ------
  
  source("code/lstm.R")
  est_model_LSTM <- lstm(Y, p.weights, treat_indices=which(rownames(Y) %in% treated.indices), d='educ_placebo_pre', t0=10, T)
  if(imp=='none'){
    LSTM_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_LSTM[rownames(est_model_LSTM)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    LSTM_ATT <- LSTM_ATT[LSTM_ATT!="NaN"]
  }else{
    LSTM_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_LSTM[rownames(est_model_LSTM)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_LSTM_c <- lstm(Y, p.weights, treat_indices=which(rownames(Y) %in% control.indices), d='educ_placebo_pre', t0=10, T) 
  
  LSTM_CI_treated <- PermutationCI(forecast=t(est_model_LSTM_c[rownames(est_model_LSTM_c)%in%control.indices,][,t0.placebo:T]), 
                                   true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]), 
                                   t.stat=LSTM_ATT,
                                   n.placebo=ncol(train_data)-1, 
                                   np=10000, 
                                   l=5000, 
                                   prec=1e-03)
  
  LSTM_CI_treated[is.infinite(LSTM_CI_treated)] <- NA
  
  ## ------
  ## ED
  ## ------
  
  source("code/ed.R")
  est_model_ED <- ed(Y, p.weights, treat_indices=which(rownames(Y) %in% treated.indices), d='educ_placebo_pre', t0=10, T)
  if(imp=='none'){
    ED_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_ED[rownames(est_model_ED)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    ED_ATT <- ED_ATT[ED_ATT!="NaN"]
  }else{
    ED_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_ED[rownames(est_model_ED)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_ED_c <- lstm(Y, p.weights, treat_indices=which(rownames(Y) %in% control.indices), d='educ_placebo_pre', t0=10, T) 
  
  ED_CI_treated <- PermutationCI(forecast=t(est_model_ED_c[rownames(est_model_ED_c)%in%control.indices,][,t0.placebo:T]), 
                                 true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]), 
                                 t.stat=ED_ATT,
                                 n.placebo=ncol(train_data)-1, 
                                 np=10000, 
                                 l=5000, 
                                 prec=1e-03)
  
  ED_CI_treated[is.infinite(ED_CI_treated)] <- NA
  
  ## -----
  ## ADH
  ## -----
  print("ADH Started")
  est_model_ADH <- adh_mp_rows(Y_obs, treat_mat)
  if(imp=='none'){
    ADH_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_ADH[rownames(est_model_ADH)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    ADH_ATT <- ADH_ATT[ADH_ATT!="NaN"]
  }else{
    ADH_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_ADH[rownames(est_model_ADH)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_ADH_c <- adh_mp_rows(Y_obs_c, treat_mat_c) 
  
  ADH_CI_treated <- PermutationCI(forecast=t(est_model_ADH_c[rownames(est_model_ADH_c)%in%control.indices,][,t0.placebo:T]), 
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]), 
                                  t.stat=ADH_ATT,
                                  n.placebo=ncol(train_data)-1, 
                                  np=10000, 
                                  l=5000, 
                                  prec=1e-03)
  
  ADH_CI_treated[is.infinite(ADH_CI_treated)] <- NA
  
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, W=p.weights, to_estimate_u = 1, to_estimate_v = 1)
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  rownames(est_model_MCPanel$Mhat) <- rownames(Y)
  colnames(est_model_MCPanel$Mhat) <- colnames(Y)
  
  if(imp=='none'){
    MCPanel_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_MCPanel$Mhat[rownames(est_model_MCPanel$Mhat)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    MCPanel_ATT <- MCPanel_ATT[MCPanel_ATT!="NaN"]
  }else{
    MCPanel_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_MCPanel$Mhat[rownames(est_model_MCPanel$Mhat)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_MCPanel_c <- mcnnm_cv(Y_obs_c, treat_mat_c, W=p.weights, to_estimate_u = 1, to_estimate_v = 1)
  est_model_MCPanel_c$Mhat <- est_model_MCPanel_c$L + replicate(T,est_model_MCPanel_c$u) + t(replicate(N,est_model_MCPanel_c$v))
  rownames(est_model_MCPanel_c$Mhat) <- rownames(treat)
  MCPanel_CI_treated <- PermutationCI(forecast=t(est_model_MCPanel_c$Mhat[rownames(est_model_MCPanel_c$Mhat)%in%control.indices,][,t0.placebo:T]), 
                                      true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]), 
                                      t.stat=MCPanel_ATT,
                                      n.placebo=ncol(train_data)-1, 
                                      np=10000, 
                                      l=5000, 
                                      prec=1e-03)
  
  MCPanel_CI_treated[is.infinite(MCPanel_CI_treated)] <- NA
  
  ## -----
  ## DID
  ## -----
  
  est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
  if(imp=='none'){
    DID_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_DID[rownames(est_model_DID)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    DID_ATT <- DID_ATT[DID_ATT!="NaN"]
  }else{
    DID_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_DID[rownames(est_model_DID)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_DID_c <- t(DID(t(Y_obs_c), t(treat_mat_c)))
  DID_CI_treated <- PermutationCI(forecast=t(est_model_DID_c[rownames(est_model_DID_c)%in%control.indices,][,t0.placebo:T]), 
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]), 
                                  t.stat=DID_ATT,
                                  n.placebo=ncol(train_data)-1, 
                                  np=10000, 
                                  l=5000, 
                                  prec=1e-03)
  
  DID_CI_treated[is.infinite(DID_CI_treated)] <- NA
  
  ## -----
  ## VT-EN: : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1))
  if(imp=='none'){
    ENT_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0.placebo:T] - est_model_ENT[rownames(est_model_ENT)%in%treated.indices,][,t0.placebo:T], na.rm=TRUE)
    ENT_ATT <- ENT_ATT[ENT_ATT!="NaN"]
  }else{
    ENT_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0.placebo:T] - est_model_ENT[rownames(est_model_ENT)%in%treated.indices,][,t0.placebo:T])
  }
  
  est_model_ENT_c <- t(en_mp_rows(t(Y_obs_c), t(treat_mat_c), num_alpha = 1))
  ENT_CI_treated <- PermutationCI(forecast=t(est_model_ENT_c[rownames(est_model_ENT_c)%in%control.indices,][,t0.placebo:T]), 
                                  true=t(Y[rownames(Y)%in%control.indices,][,t0.placebo:T]), 
                                  t.stat=ENT_ATT,
                                  n.placebo=ncol(train_data)-1, 
                                  np=10000, 
                                  l=5000, 
                                  prec=1e-03)
  
  ENT_CI_treated[is.infinite(ENT_CI_treated)] <- NA
  
  ## Saving data
  if(if.save){
    df1 <-
      data.frame(
        "y" =  c(mean(DID_ATT),mean(ED_ATT),mean(LSTM_ATT),mean(MCPanel_ATT),mean(ADH_ATT),mean(ENT_ATT)),
        "CI.lower" = c(mean(DID_CI_treated[,1], na.rm=TRUE),mean(ED_CI_treated[,1], na.rm=TRUE),mean(LSTM_CI_treated[,1], na.rm=TRUE),mean(MCPanel_CI_treated[,1], na.rm=TRUE),mean(ADH_CI_treated[,1], na.rm=TRUE),mean(ENT_CI_treated[,1], na.rm=TRUE)),
        "CI.upper" = c(mean(DID_CI_treated[,2], na.rm=TRUE),mean(ED_CI_treated[,2], na.rm=TRUE),mean(LSTM_CI_treated[,2], na.rm=TRUE),mean(MCPanel_CI_treated[,2], na.rm=TRUE),mean(ADH_CI_treated[,2], na.rm=TRUE),mean(ENT_CI_treated[,2], na.rm=TRUE)),
        "x" = rep(t0.placebo, 6),
        "Method" = c("DID", 
                     "Encoder-decoder",
                     "LSTM",
                     "MC-NNM", 
                     "SCM",
                     "SCM-L1"))
    
    filename <- paste0("educ-placebo-",imp,t0.placebo,sim,".rds")
    print(filename)
    save(df1, file = paste0("results/",filename))
  }
}

# Read data
capacity.outcomes <- readRDS("data/capacity-outcomes-locf.rds")

# Transform covars to unit and time-specific inputs
capacity.covars <- cbind(capacity.outcomes$educ.pc$faval[,c("1850","1860")], 
                         capacity.outcomes$educ.pc$farmsize[,c("1860")],
                         capacity.outcomes$educ.pc$access[,c("1860")])

capacity.covars <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
capacity.covars[is.na(capacity.covars)] <- 0

CapacitySim(outcomes=capacity.outcomes,covars.x=capacity.covars, d="educ.pc",sim=0, imp="locf", if.save=TRUE)