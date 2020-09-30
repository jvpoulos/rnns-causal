######################################################################
# Education Spending Application: Comparison estimators #
######################################################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(boot)
library(readr)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

source("code/permutationTest.R")

CapacityCompare <- function(imp=c("none","locf","linear","random","mean","ma"), run.CI=TRUE, if.save=TRUE){
  
  # Read data
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  capacity.covariates <- readRDS("data/capacity-covariates.rds")
  
  print(dim(capacity.outcomes$educ.pc$M))
  
  # Transform covars to unit and time-specific inputs
  common_rows <- intersect(intersect(rownames(capacity.covariates$faval), rownames(capacity.covariates$farmsize)), rownames(capacity.covariates$access))
  capacity.covars <- cbind(capacity.covariates$faval[,c("faval.1850","faval.1860")][common_rows,], 
                           capacity.covariates$farmsize[,c("farmsize.1790", "farmsize.1800", "farmsize.1810", "farmsize.1820", "farmsize.1830", "farmsize.1840", "farmsize.1850", "farmsize.1860")][common_rows,],
                           capacity.covariates$access[,-c(1)][common_rows,])
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  train_data <- read_csv(paste0("data/educ-x-",imp,".csv"), col_names = TRUE)
  test_data <- read_csv(paste0("data/educ-y-",imp,".csv"), col_names = TRUE)
  
  treated.indices <- colnames(test_data)
  control.indices <- colnames(train_data)
  
  # Prepare outcomes data
  educ <- capacity.outcomes$educ.pc
  treat <- educ$mask # NxT masked matrix 
  Y.missing <- educ$M.missing # NxT
  Y <- educ$M # NxT 
  
  # Parity
  treat <- treat[rownames(treat)%in%c(colnames(train_data),colnames(test_data)),]
  Y.missing <- Y.missing[rownames(Y.missing)%in%c(colnames(train_data),colnames(test_data)),]
  Y <- Y[rownames(Y)%in%c(colnames(train_data),colnames(test_data)),]
  capacity.covars <- capacity.covars[rownames(capacity.covars)%in%c(colnames(train_data),colnames(test_data)),]
  
  if(imp=="none"){
    Y <- Y[, - as.numeric(which(apply(Y, 2, var) == 0))] # rm col from 0 variance
    Y.missing <- Y.missing[,colnames(Y.missing) %in% colnames(Y)]
    treat <- treat[,colnames(treat) %in% colnames(Y)]
    capacity.covars <- capacity.covars[,colnames(capacity.covars) %in% colnames(Y)]
    
    Y_imp <- Y * Y.missing # use for calculating RMSE on non-imputed values
  }
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  t0 <- which(colnames(Y)=="1869")
  
  # Censor post-period treated values
  
  treat_mat <- 1-treat # treated are 0
  
  Y_obs <- Y * treat_mat # treated are 0
  
  # For randomization CIs
  treat_mat_c <- simul_adapt(Y, N_t = length(control.indices), T0=t0, treat_indices = which(rownames(Y) %in% control.indices))
  Y_obs_c <- Y * treat_mat_c
  
  ## Estimate propensity scores
  
  p.mod <- glmnet(x=cbind(capacity.covars,Y_obs[,1:(t0-1)]), y=(1-treat_mat), family="mgaussian", alpha=1, nlambda = 5) # avoid CV
  W <- predict(p.mod, cbind(capacity.covars,Y_obs[,1:(t0-1)]))[,,1]
  W[,1:(t0-1)] <- W[,t0] # assume pre-treatment W same as t0
  W[W <=0.01] <- 0.01 # threshold values
  W[W >=1] <- 1-0.01 # threshold values
  
  p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
  p.weights <- treat*(1-W) + (1-treat)*(W)
  
  ## ------
  ## LSTM
  ## ------
  
  T.final <- which(colnames(Y)=="1942") # calc ATT from 1869-1942
  
  source("code/lstm.R")
  est_model_LSTM <- lstm(Y_obs, p.weights, treat_indices=which(rownames(Y_obs) %in% treated.indices), d='educ_benchmark', t0=20, T)
  if(imp=='none'){
    LSTM_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_LSTM[rownames(est_model_LSTM)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    LSTM_ATT <- LSTM_ATT[LSTM_ATT!="NaN"]
  }else{
    LSTM_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_LSTM[rownames(est_model_LSTM)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_LSTM_c <- lstm(Y_obs_c, p.weights, treat_indices=which(rownames(Y_obs) %in% control.indices), d='educ_benchmark', t0=20, T) 
    
    LSTM_CI_treated <- PermutationCI(forecast=t(est_model_LSTM_c[rownames(est_model_LSTM_c)%in%control.indices,][,t0:T]), 
                                     true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                     t.stat=LSTM_ATT,
                                     n.placebo=ncol(train_data)-1, 
                                     np=10000, 
                                     l=500, 
                                     prec=1e-03)
    
    LSTM_CI_treated[is.infinite(LSTM_CI_treated)] <- NA
    
    saveRDS(LSTM_CI_treated, paste0("results/", "LSTM-CI-treated-",imp,".rds"))
  } else{
    LSTM_CI_treated <- readRDS(paste0("results/", "LSTM-CI-treated-",imp,".rds"))
    rownames(LSTM_CI_treated) <- rownames(t(est_model_LSTM[,t0:T]))
    print(paste0("ATT:", mean(LSTM_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(LSTM_CI_treated[,1][which(rownames(LSTM_CI_treated)=="1869"):which(rownames(LSTM_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(LSTM_CI_treated[,2][which(rownames(LSTM_CI_treated)=="1869"):which(rownames(LSTM_CI_treated)=="1942")])))
  }
  
  ## ------
  ## ED
  ## ------
  
  source("code/ed.R")
  est_model_ED <- ed(Y_obs, p.weights, treat_indices=which(rownames(Y_obs) %in% treated.indices), d='educ_benchmark', t0=20, T)
  if(imp=='none'){
    ED_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_ED[rownames(est_model_ED)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    ED_ATT <- ED_ATT[ED_ATT!="NaN"]
  }else{
    ED_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_ED[rownames(est_model_ED)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_ED_c <- lstm(Y_obs_c, p.weights, treat_indices=which(rownames(Y_obs) %in% control.indices), d='educ_benchmark', t0=20, T) 
    
    ED_CI_treated <- PermutationCI(forecast=t(est_model_ED_c[rownames(est_model_ED_c)%in%control.indices,][,t0:T]), 
                                   true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                   t.stat=ED_ATT,
                                   n.placebo=ncol(train_data)-1, 
                                   np=10000, 
                                   l=500, 
                                   prec=1e-03)
    
    ED_CI_treated[is.infinite(ED_CI_treated)] <- NA
    
    saveRDS(ED_CI_treated, paste0("results/", "ED-CI-treated-",imp,".rds"))
  } else{
    ED_CI_treated <- readRDS(paste0("results/", "ED-CI-treated-",imp,".rds"))
    rownames(ED_CI_treated) <- rownames(t(est_model_ED[,t0:T]))
    print(paste0("ATT:", mean(ED_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(ED_CI_treated[,1][which(rownames(ED_CI_treated)=="1869"):which(rownames(ED_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(ED_CI_treated[,2][which(rownames(ED_CI_treated)=="1869"):which(rownames(ED_CI_treated)=="1942")])))
  }
  
  ## -----
  ## ADH
  ## -----
  print("ADH Started")
  source("code/ADH.R") # clip gradients
  est_model_ADH <- adh_mp_rows(Y_obs, treat_mat) # defaults
  if(imp=='none'){
    ADH_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_ADH[rownames(est_model_ADH)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    ADH_ATT <- ADH_ATT[ADH_ATT!="NaN"]
  }else{
    ADH_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_ADH[rownames(est_model_ADH)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_ADH_c <- adh_mp_rows(Y_obs_c, treat_mat_c) 
    
    ADH_CI_treated <- PermutationCI(forecast=t(est_model_ADH_c[rownames(est_model_ADH_c)%in%control.indices,][,t0:T]), 
                                    true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                    t.stat=ADH_ATT,
                                    n.placebo=ncol(train_data)-1, 
                                    np=10000, 
                                    l=500, 
                                    prec=1e-03)
    
    ADH_CI_treated[is.infinite(ADH_CI_treated)] <- NA
    
    saveRDS(ADH_CI_treated, paste0("results/", "ADH-CI-treated-",imp,".rds"))
  } else{
    ADH_CI_treated <- readRDS(paste0("results/", "ADH-CI-treated-",imp,".rds"))
    rownames(ADH_CI_treated) <- rownames(t(est_model_ADH[,t0:T]))
    print(paste0("ATT:", mean(ADH_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(ADH_CI_treated[,1][which(rownames(ADH_CI_treated)=="1869"):which(rownames(ADH_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(ADH_CI_treated[,2][which(rownames(ADH_CI_treated)=="1869"):which(rownames(ADH_CI_treated)=="1942")])))
  }
  
  ## ------
  ## VAR
  ## ------
  
  source("code/varEst.R")
  est_model_VAR <- varEst(Y_obs, treat_indices=treated.indices)
  if(imp=='none'){
    VAR_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_VAR[rownames(est_model_VAR)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    VAR_ATT <- VAR_ATT[VAR_ATT!="NaN"]
  }else{
    VAR_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_VAR[rownames(est_model_VAR)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_VAR_c <- varEst(Y_obs_c, treat_indices=control.indices) 
    
    VAR_CI_treated <- PermutationCI(forecast=t(est_model_VAR_c[rownames(est_model_VAR_c)%in%control.indices,][,t0:T]), 
                                    true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                    t.stat=VAR_ATT,
                                    n.placebo=ncol(train_data)-1, 
                                    np=10000, 
                                    l=500, 
                                    prec=1e-03)
    
    VAR_CI_treated[is.infinite(VAR_CI_treated)] <- NA
    
    saveRDS(VAR_CI_treated, paste0("results/", "VAR-CI-treated-",imp,".rds"))
  } else{
    VAR_CI_treated <- readRDS(paste0("results/", "VAR-CI-treated-",imp,".rds"))
    rownames(VAR_CI_treated) <- rownames(t(est_model_VAR[,t0:T]))
    print(paste0("ATT:", mean(VAR_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(VAR_CI_treated[,1][which(rownames(VAR_CI_treated)=="1869"):which(rownames(VAR_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(VAR_CI_treated[,2][which(rownames(VAR_CI_treated)=="1869"):which(rownames(VAR_CI_treated)=="1942")])))
  }
  
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, W=p.weights, to_estimate_u = 1, to_estimate_v = 1)
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  rownames(est_model_MCPanel$Mhat) <- rownames(Y)
  colnames(est_model_MCPanel$Mhat) <- colnames(Y)
  
  if(imp=='none'){
    MCPanel_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_MCPanel$Mhat[rownames(est_model_MCPanel$Mhat)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    MCPanel_ATT <- MCPanel_ATT[MCPanel_ATT!="NaN"]
  }else{
    MCPanel_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_MCPanel$Mhat[rownames(est_model_MCPanel$Mhat)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_MCPanel_c <- mcnnm_cv(Y_obs_c, treat_mat_c, W=p.weights, to_estimate_u = 1, to_estimate_v = 1)
    est_model_MCPanel_c$Mhat <- est_model_MCPanel_c$L + replicate(T,est_model_MCPanel_c$u) + t(replicate(N,est_model_MCPanel_c$v))
    rownames(est_model_MCPanel_c$Mhat) <- rownames(treat)
    MCPanel_CI_treated <- PermutationCI(forecast=t(est_model_MCPanel_c$Mhat[rownames(est_model_MCPanel_c$Mhat)%in%control.indices,][,t0:T]), 
                                        true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                        t.stat=MCPanel_ATT,
                                        n.placebo=ncol(train_data)-1, 
                                        np=10000, 
                                        l=500, 
                                        prec=1e-03)
    
    MCPanel_CI_treated[is.infinite(MCPanel_CI_treated)] <- NA
    
    saveRDS(MCPanel_CI_treated, paste0("results/", "MCPanel-CI-treated-",imp,".rds"))
  } else{
    MCPanel_CI_treated <- readRDS(paste0("results/", "MCPanel-CI-treated-",imp,".rds"))
    rownames(MCPanel_CI_treated) <- rownames(t(est_model_MCPanel$Mhat[,t0:T]))
    print(paste0("ATT:", mean(MCPanel_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(MCPanel_CI_treated[,1][which(rownames(MCPanel_CI_treated)=="1869"):which(rownames(MCPanel_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(MCPanel_CI_treated[,2][which(rownames(MCPanel_CI_treated)=="1869"):which(rownames(MCPanel_CI_treated)=="1942")])))
  }
  
  ## -----
  ## DID
  ## -----
  
  est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
  if(imp=='none'){
    DID_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_DID[rownames(est_model_DID)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    DID_ATT <- DID_ATT[DID_ATT!="NaN"]
  }else{
    DID_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_DID[rownames(est_model_DID)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_DID_c <- t(DID(t(Y_obs_c), t(treat_mat_c)))
    DID_CI_treated <- PermutationCI(forecast=t(est_model_DID_c[rownames(est_model_DID_c)%in%control.indices,][,t0:T]), 
                                    true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                    t.stat=DID_ATT,
                                    n.placebo=ncol(train_data)-1, 
                                    np=10000, 
                                    l=500, 
                                    prec=1e-03)
    
    DID_CI_treated[is.infinite(DID_CI_treated)] <- NA
    
    saveRDS(DID_CI_treated, paste0("results/", "DID-CI-treated-",imp,".rds"))
  } else{
    DID_CI_treated <- readRDS(paste0("results/", "DID-CI-treated-",imp,".rds"))
    rownames(DID_CI_treated) <- rownames(t(est_model_DID[,t0:T]))
    print(paste0("ATT:", mean(DID_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(DID_CI_treated[,1][which(rownames(DID_CI_treated)=="1869"):which(rownames(DID_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(DID_CI_treated[,2][which(rownames(DID_CI_treated)=="1869"):which(rownames(DID_CI_treated)=="1942")])))
  }
  
  ## -----
  ## VT-EN: : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1))
  if(imp=='none'){
    ENT_ATT <- colMeans(Y_imp[rownames(Y_imp)%in%treated.indices,][,t0:T.final] - est_model_ENT[rownames(est_model_ENT)%in%treated.indices,][,t0:T.final], na.rm=TRUE)
    ENT_ATT <- ENT_ATT[ENT_ATT!="NaN"]
  }else{
    ENT_ATT <- colMeans(Y[rownames(Y)%in%treated.indices,][,t0:T.final] - est_model_ENT[rownames(est_model_ENT)%in%treated.indices,][,t0:T.final])
  }
  
  if(run.CI){
    est_model_ENT_c <- t(en_mp_rows(t(Y_obs_c), t(treat_mat_c), num_alpha = 1))
    ENT_CI_treated <- PermutationCI(forecast=t(est_model_ENT_c[rownames(est_model_ENT_c)%in%control.indices,][,t0:T]), 
                                    true=t(Y[rownames(Y)%in%control.indices,][,t0:T]), 
                                    t.stat=ENT_ATT,
                                    n.placebo=ncol(train_data)-1, 
                                    np=10000, 
                                    l=500, 
                                    prec=1e-03)
    
    ENT_CI_treated[is.infinite(ENT_CI_treated)] <- NA
    
    saveRDS(ENT_CI_treated, paste0("results/", "ENT-CI-treated-",imp,".rds"))
  } else{
    ENT_CI_treated <- readRDS(paste0("results/", "ENT-CI-treated-",imp,".rds"))
    rownames(ENT_CI_treated) <- rownames(t(est_model_ENT[,t0:T]))
    print(paste0("ATT:", mean(ENT_ATT))) # avg over post-treatment period
    print(paste0("CI lower:", mean(ENT_CI_treated[,1][which(rownames(ENT_CI_treated)=="1869"):which(rownames(ENT_CI_treated)=="1942")])))
    print(paste0("CI upper:", mean(ENT_CI_treated[,2][which(rownames(ENT_CI_treated)=="1869"):which(rownames(ENT_CI_treated)=="1942")])))
  }
  
  ## Saving data
  if(if.save){
    df1 <-
      data.frame(
        "y" =  c(mean(DID_ATT),mean(ED_ATT),mean(LSTM_ATT),mean(MCPanel_ATT),mean(ADH_ATT),mean(ENT_ATT),mean(VAR_ATT)),
        "CI.lower" = c(mean(DID_CI_treated[,1], na.rm=TRUE),mean(ED_CI_treated[,1], na.rm=TRUE),mean(LSTM_CI_treated[,1], na.rm=TRUE),mean(MCPanel_CI_treated[,1], na.rm=TRUE),mean(ADH_CI_treated[,1], na.rm=TRUE),mean(ENT_CI_treated[,1], na.rm=TRUE),mean(VAR_CI_treated[,1], na.rm=TRUE)),
        "CI.upper" = c(mean(DID_CI_treated[,2], na.rm=TRUE),mean(ED_CI_treated[,2], na.rm=TRUE),mean(LSTM_CI_treated[,2], na.rm=TRUE),mean(MCPanel_CI_treated[,2], na.rm=TRUE),mean(ADH_CI_treated[,2], na.rm=TRUE),mean(ENT_CI_treated[,2], na.rm=TRUE),mean(VAR_CI_treated[,2], na.rm=TRUE)),
        "Method" = c("DID", 
                     "Encoder-decoder",
                     "LSTM",
                     "MC-NNM", 
                     "SCM",
                     "SCM-L1",
                     "VAR"))
    
    filename <- paste0("educ-benchmark-compare-",imp,".rds")
    save(df1, file = paste0("results/",filename))
  }
}

for(imp in c("locf","linear","random","mean","ma")){
  CapacityCompare(imp=imp, run.CI = TRUE, if.save=TRUE)
}