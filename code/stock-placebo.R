###################################################
# Stock Market Data Simulations #
###################################################

## Loading Source files
library(MCPanel)
library(glmnet)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

StockSim <- function(Y,N,T,sim){
  ## Setting up the configuration
  Nbig <- nrow(Y)
  
  N <- N
  T <- T
  
  t0 <- ceiling(T/2) # time of initial treatment
  N_t <- ceiling(N/2)
  num_runs <- 100
  is_simul <- sim ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  d <- 'stock'

  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs)
  VAR_RMSE_test <- matrix(0L,num_runs)
  LSTM_RMSE_test <- matrix(0L,num_runs)
  ED_RMSE_test <- matrix(0L,num_runs)
  DID_RMSE_test <- matrix(0L,num_runs)
  ADH_RMSE_test <- matrix(0L,num_runs)
  ENT_RMSE_test <- matrix(0L,num_runs)
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    all_indices <- sort(sample(1:Nbig, N))
    treat_indices <- sort(sample(1:N, N_t))
    Y_sub <- Y[all_indices,1:T]
    ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
    if(is_simul == 1){
      treat_mat <- simul_adapt(Y_sub, N_t, (t0-1), treat_indices)
    }else{
      treat_mat <- stag_adapt(Y_sub, N_t, (t0-1), treat_indices)
    }
    
    Y_obs <- Y_sub * treat_mat

    ## Estimate propensity scores

    p.mod <- cv.glmnet(x=Y_obs, y=(1-treat_mat), family="mgaussian", parallel = TRUE) # LOO
    
    p.weights <- predict(p.mod, Y_obs, type="response", s = "lambda.min")[,,1]
    
    ## -----
    ## ADH
    ## -----
    
    print("ADH Started")
    est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter=200, rel_tol = 0.001)
    est_model_ADH_msk_err <- (est_model_ADH - Y_sub)*(1-treat_mat)
    est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
    ADH_RMSE_test[i] <- est_model_ADH_test_RMSE
    print(paste("ADH RMSE:", round(est_model_ADH_test_RMSE,3),"run",i))
    
    ## ------
    ## ED
    ## ------
    
    print("ED Started")
    source("code/ed.R")
    est_model_ED <- ed(Y=Y_obs, p.weights, treat_indices, d, t0, T)
    est_model_ED_msk_err <- (est_model_ED - Y_sub[treat_indices,][,t0:T])
    est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
    ED_RMSE_test[i] <- est_model_ED_test_RMSE
    print(paste("ED RMSE:", round(est_model_ED_test_RMSE,3),"run",i))
    
    ## ------
    ## LSTM
    ## ------
    
    print("LSTM Started")
    source("code/lstm.R")
    est_model_LSTM <- lstm(Y=Y_obs, p.weights, treat_indices, d, t0, T)
    est_model_LSTM_msk_err <- (est_model_LSTM - Y_sub[treat_indices,][,t0:T])
    est_model_LSTM_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err^2, na.rm = TRUE))
    LSTM_RMSE_test[i] <- est_model_LSTM_test_RMSE
    print(paste("LSTM RMSE:", round(est_model_LSTM_test_RMSE,3),"run",i))
    
    ## ------
    ## VAR
    ## ------
    
    print("VAR Started")
    source("code/varEst.R")
    est_model_VAR <- varEst(Y=Y_obs, treat_indices, t0, T)
    est_model_VAR_msk_err <- (est_model_VAR - Y_sub[treat_indices,])
    est_model_VAR_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_VAR_msk_err^2, na.rm = TRUE))
    VAR_RMSE_test[i] <- est_model_VAR_test_RMSE
    print(paste("VAR RMSE:", round(est_model_VAR_test_RMSE,3),"run",i))
    
    ## ------
    ## MC-NNM
    ## ------
    
    print("MC-NNM Started")
    est_model_MCPanel <- mcnnm(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, lambda_L = c(0.2), niter = 200)[[1]] # no CV to save computational time
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y_sub)*(1-treat_mat)
    est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
    MCPanel_RMSE_test[i] <- est_model_MCPanel$test_RMSE
    print(paste("MC-NNM RMSE:", round(est_model_MCPanel$test_RMSE,3),"run",i))
    
    ## -----
    ## DID
    ## -----
    
    print("DID Started")
    est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
    est_model_DID_msk_err <- (est_model_DID - Y_sub)*(1-treat_mat)
    est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
    DID_RMSE_test[i] <- est_model_DID_test_RMSE
    print(paste("DID RMSE:", round(est_model_DID_test_RMSE,3),"run",i))
    
    ## -----
    ## VT-EN 
    ## -----
    
    est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_folds = 3))
    est_model_ENT_msk_err <- (est_model_ENT - Y_sub)*(1-treat_mat)
    est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2, na.rm = TRUE))
    ENT_RMSE_test[i] <- est_model_ENT_test_RMSE
    print(paste("VT-EN RMSE:", round(est_model_ENT_test_RMSE,3),"run",i))
  }
  
  ## Computing means and standard errors
  
  MCPanel_avg_RMSE <- apply(MCPanel_RMSE_test,2,mean)
  MCPanel_std_error <- apply(MCPanel_RMSE_test,2,sd)/sqrt(num_runs)
  
  VAR_avg_RMSE <- apply(VAR_RMSE_test,2,mean)
  VAR_std_error <- apply(VAR_RMSE_test,2,sd)/sqrt(num_runs)
  
  LSTM_avg_RMSE <- apply(LSTM_RMSE_test,2,mean)
  LSTM_std_error <- apply(LSTM_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED_avg_RMSE <- apply(ED_RMSE_test,2,mean)
  ED_std_error <- apply(ED_RMSE_test,2,sd)/sqrt(num_runs)
  
  DID_avg_RMSE <- apply(DID_RMSE_test,2,mean)
  DID_std_error <- apply(DID_RMSE_test,2,sd)/sqrt(num_runs)
  
  ADH_avg_RMSE <- apply(ADH_RMSE_test,2,mean)
  ADH_std_error <- apply(ADH_RMSE_test,2,sd)/sqrt(num_runs)
  
  ENT_avg_RMSE <- apply(ENT_RMSE_test,2,mean)
  ENT_std_error <- apply(ENT_RMSE_test,2,sd)/sqrt(num_runs)
  
  ## Saving data
  
  df1 <-
    data.frame(
      "y" =  c(DID_avg_RMSE,ED_avg_RMSE,LSTM_avg_RMSE,MCPanel_avg_RMSE,ADH_avg_RMSE,VAR_avg_RMSE,ENT_avg_RMSE),
      "lb" = c(DID_avg_RMSE - 1.96*DID_std_error,
               ED_avg_RMSE - 1.96*ED_std_error,
               LSTM_avg_RMSE - 1.96*LSTM_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error, 
               ADH_avg_RMSE - 1.96*ADH_std_error,
               VAR_avg_RMSE - 1.96*VAR_std_error,
               ENT_avg_RMSE - 1.96*ENT_std_error),
      "ub" = c(DID_avg_RMSE + 1.96*DID_std_error, 
               ED_avg_RMSE + 1.96*ED_std_error,
               LSTM_avg_RMSE + 1.96*LSTM_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error, 
               ADH_avg_RMSE + 1.96*ADH_std_error,
               VAR_avg_RMSE + 1.96*VAR_std_error,
               ENT_avg_RMSE + 1.96*ENT_std_error),
      "x" = replicate(7,N*T),
      "Method" = c("DID", 
                   "Encoder-decoder",
                   "LSTM", 
                   "MC-NNM", 
                   "SCM",
                   "VAR",
                   "Vertical"))
  
  ##
  filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
  saveRDS(df1, file = paste0("results/",filename))
}

# Load data
Y <- t(read.csv('data/returns_no_missing.csv',header=F)) # N X T

print(paste0("N X T data dimension: ", dim(Y)))

for(i in c(0,1)){
  # Fixed NT: 20,000
  # StockSim(Y,N=1000,T=20,sim=i) 
  # StockSim(Y,N=200,T=100,sim=i) 
  StockSim(Y,N=160,T=125,sim=i) 
  StockSim(Y,N=100,T=200,sim=i) 
  StockSim(Y,N=20,T=1000,sim=i) 
}

StockSim(Y,N=1000,T=20,sim=1) 
StockSim(Y,N=200,T=100,sim=1) 