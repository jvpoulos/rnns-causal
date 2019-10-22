###################################
# Stock Market Data Simulations #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(ggplot2)
library(latex2exp)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()/2

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

StockSim <- function(Y,N,fix_d){
  ## Setting up the configuration
  Nbig <- nrow(Y)
  Tbig <- ncol(Y)
  
  N <- N
  if(fix_d){
    T <- 49000/N
  }else{
    T <- (Tbig/Nbig) * N
  }
  
  T0 <- ceiling(T/2)
  N_t <- ceiling(N/2)
  num_runs <- 10
  is_simul <- 1 ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  if(fix_d){
    d <- 'stock_fixed'
  }else{
    d <- 'stock'
  }
  
  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
  LSTM_RMSE_test <- matrix(0L,num_runs,length(T0))
  RVAE_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED_RMSE_test <- matrix(0L,num_runs,length(T0))
  ENT_RMSE_test <- matrix(0L,num_runs,length(T0))
  DID_RMSE_test <- matrix(0L,num_runs,length(T0))
  ADH_RMSE_test <- matrix(0L,num_runs,length(T0))
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    all_indices <- sample(1:Nbig, N)
    treat_indices <- sample(1:N, N_t)
    Y_sub <- Y[all_indices,1:T]
    for (j in c(1:length(T0))){
      treat_mat <- matrix(1L, N, T) # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
      t0 <- T0[j]
      ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
      if(is_simul == 1){
        treat_mat <- simul_adapt(Y_sub, N_t, t0, treat_indices)
      }else{
        treat_mat <- stag_adapt(Y_sub, N_t, t0, treat_indices)
      }
      treat_mat_NA <- treat_mat
      treat_mat_NA[treat_mat==0] <- NA
      
      Y_obs <- Y_sub * treat_mat
      
      ## ------
      ## LSTM
      ## ------
      
      print("LSTM Started")
      source("code/lstm.R")
      est_model_LSTM <- lstm(Y_obs, treat_indices, d, t0, T)
      est_model_LSTM_msk_err <- (est_model_LSTM - Y_sub[treat_indices,][,(t0+1):T])
      est_model_LSTM_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err^2, na.rm = TRUE))
      LSTM_RMSE_test[i,j] <- est_model_LSTM_test_RMSE
      
      ## ------
      ## RVAE
      ## ------
      
      print("RVAE Started")
      source("code/rvae.R")
      est_model_RVAE <- rvae(Y_obs, treat_indices, d, t0, T)
      est_model_RVAE_msk_err <- (est_model_RVAE - Y_sub[treat_indices,][,(t0+1):T])
      est_model_RVAE_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_RVAE_msk_err^2, na.rm = TRUE))
      RVAE_RMSE_test[i,j] <- est_model_RVAE_test_RMSE
      
      ## ------
      ## ED
      ## ------
      
      print("ED Started")
      source("code/ed.R")
      est_model_ED <- ed(Y_obs, treat_indices, d, t0, T)
      est_model_ED_msk_err <- (est_model_ED - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
      ED_RMSE_test[i,j] <- est_model_ED_test_RMSE
      
      ## ------
      ## MC-NNM
      ## ------
      
      print("MC-NNM Started")
      est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 2)
      est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
      est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y_sub)*(1-treat_mat)
      est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
      MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE
      
      ## -----
      ## VT-EN 
      ## -----
      
      print("VT-EN Started")
      est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat)))
      est_model_ENT_msk_err <- (est_model_ENT - Y_sub)*(1-treat_mat)
      est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2, na.rm = TRUE))
      ENT_RMSE_test[i,j] <- est_model_ENT_test_RMSE
      
      ## -----
      ## DID
      ## -----
      
      print("DID Started")
      est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
      est_model_DID_msk_err <- (est_model_DID - Y_sub)*(1-treat_mat)
      est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
      DID_RMSE_test[i,j] <- est_model_DID_test_RMSE
      
      ## -----
      ## ADH
      ## -----
      
      print("ADH Started")
      est_model_ADH <- adh_mp_rows(Y_obs, treat_mat)
      est_model_ADH_msk_err <- (est_model_ADH - Y_sub)*(1-treat_mat)
      est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
      ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
    }
  }
  
  ## Computing means and standard errors
  
  MCPanel_avg_RMSE <- apply(MCPanel_RMSE_test,2,mean)
  MCPanel_std_error <- apply(MCPanel_RMSE_test,2,sd)/sqrt(num_runs)
  
  LSTM_avg_RMSE <- apply(LSTM_RMSE_test,2,mean)
  LSTM_std_error <- apply(LSTM_RMSE_test,2,sd)/sqrt(num_runs)
  
  RVAE_avg_RMSE <- apply(RVAE_RMSE_test,2,mean)
  RVAE_std_error <- apply(RVAE_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED_avg_RMSE <- apply(ED_RMSE_test,2,mean)
  ED_std_error <- apply(ED_RMSE_test,2,sd)/sqrt(num_runs)
  
  ENT_avg_RMSE <- apply(ENT_RMSE_test,2,mean)
  ENT_std_error <- apply(ENT_RMSE_test,2,sd)/sqrt(num_runs)
  
  DID_avg_RMSE <- apply(DID_RMSE_test,2,mean)
  DID_std_error <- apply(DID_RMSE_test,2,sd)/sqrt(num_runs)
  
  ADH_avg_RMSE <- apply(ADH_RMSE_test,2,mean)
  ADH_std_error <- apply(ADH_RMSE_test,2,sd)/sqrt(num_runs)
  
  ## Creating plots
  
  df1 <-
    data.frame(
      "y" =  c(DID_avg_RMSE,ED_avg_RMSE,LSTM_avg_RMSE,MCPanel_avg_RMSE,RVAE_avg_RMSE,ADH_avg_RMSE,ENT_avg_RMSE),
      "lb" = c(DID_avg_RMSE - 1.96*DID_std_error,
               ED_avg_RMSE - 1.96*ED_std_error,
               LSTM_avg_RMSE - 1.96*LSTM_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error, 
               RVAE_avg_RMSE - 1.96*RVAE_std_error, 
               ADH_avg_RMSE - 1.96*ADH_std_error,
               ENT_avg_RMSE - 1.96*ENT_std_error),
      "ub" = c(DID_avg_RMSE + 1.96*DID_std_error, 
               ED_avg_RMSE + 1.96*ED_std_error,
               LSTM_avg_RMSE + 1.96*LSTM_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error, 
               RVAE_avg_RMSE + 1.96*RVAE_std_error, 
               ADH_avg_RMSE + 1.96*ADH_std_error,
               ENT_avg_RMSE + 1.96*ENT_std_error),
      "N" = rep(N,7),
      "T" = rep(T,7),
      "Method" = c(replicate(length(T0),"DID"), 
                   replicate(length(T0),"ED"),
                   replicate(length(T0),"LSTM"), 
                   replicate(length(T0),"MC-NNM"), 
                   replicate(length(T0),"RVAE"), 
                   replicate(length(T0),"SC-ADH"),
                   replicate(length(T0),"VT-EN")))
  filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
  saveRDS(df1, file = paste0("results/",filename))
  return(df1)
}

# Load data
Y <- t(read.csv('data/returns_no_missing.csv',header=F)) # N X T

fixed.dimensions <- TRUE
if(fixed.dimensions){
  # fixed dimensions
  results <- foreach(N = c(100,200,500,700,1000,1400), .combine='rbind') %do% {
    StockSim(Y,N, fix_d=TRUE)
  }
  saveRDS(results, "results/stock-placebo-results-fixed.rds")
} else{
  # increase dimensions
  results <- foreach(N = c(100,200,500,700,1000,1400), .combine='rbind') %do% {
    StockSim(Y,N, fix_d=FALSE)
  }
  saveRDS(results, "results/stock-placebo-results-inc.rds")
}