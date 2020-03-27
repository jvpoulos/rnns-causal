###################################################
# Product Sales Data Simulations: Fixed T, Varying N#
###################################################

## Loading Source files
library(MCPanel)
library(glmnet)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- 2

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

SalesSim <- function(Y,N,sim){
  ## Setting up the configuration
  Nbig <- nrow(Y)
  Tbig <- ncol(Y)
  
  N <- N
  T <- Tbig
  
  t0 <- ceiling(T/2) # time of initial treatment
  N_t <- ceiling(N/2)
  num_runs <- 25
  is_simul <- sim ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  d <- 'sales_fixed'

  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs)
  VAR_RMSE_test <- matrix(0L,num_runs)
  LSTM_RMSE_test <- matrix(0L,num_runs)
  ED_RMSE_test <- matrix(0L,num_runs)
  DID_RMSE_test <- matrix(0L,num_runs)
  ADH_RMSE_test <- matrix(0L,num_runs)
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    all_indices <- sample(1:Nbig, N)
    treat_indices <- sample(1:N, N_t)
    Y_sub <- Y[all_indices,1:T]
    ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
    if(is_simul == 1){
      treat_mat <- simul_adapt(Y_sub, N_t, (t0-1), treat_indices)
    }else{
      treat_mat <- stag_adapt(Y_sub, N_t, (t0-1), treat_indices)
    }
    
    Y_obs <- Y_sub * treat_mat
    
    ## Estimate propensity scores
    
    logitMod.x <- cv.glmnet(x=Y_obs, y=as.factor((1-treat_mat)[,t0]), family="binomial", nfolds=nrow(Y_obs), parallel = TRUE, nlambda=400) # LOO
    
    logitMod.z <- cv.glmnet(x=t(Y_obs), y=as.factor((1-treat_mat)[treat_indices[1],]), family="binomial", nfolds=nrow(t(Y_obs)), parallel = TRUE, nlambda=400)
    
    p.weights.x <- as.vector(predict(logitMod.x, Y_obs, type="response", s = "lambda.min"))
    p.weights.z <- as.vector(predict(logitMod.z, t(Y_obs), type="response", s = "lambda.min"))
    
    p.weights <- outer(p.weights.x,p.weights.z)   # outer product of fitted values on response scale
    
    ## -----
    ## ADH
    ## -----
    
    print("ADH Started")
    est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter=200)
    est_model_ADH[est_model_ADH <0] <- 0
    est_model_ADH <- round(est_model_ADH)
    est_model_ADH_msk_err <- (est_model_ADH - Y_sub)*(1-treat_mat)
    est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
    ADH_RMSE_test[i] <- est_model_ADH_test_RMSE
    print(paste("ADH RMSE:", round(est_model_ADH_test_RMSE,3),"run",i))
    
    ## ------
    ## ED
    ## ------
    
    print("ED Started")
    source("code/ed.R")
    est_model_ED <- ed(Y=Y_sub, p.weights, treat_indices, d, t0, T)
    est_model_ED[est_model_ED <0] <- 0
    est_model_ED <- round(est_model_ED)
    est_model_ED_msk_err <- (est_model_ED - Y_sub[treat_indices,][,t0:T])
    est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
    ED_RMSE_test[i] <- est_model_ED_test_RMSE
    print(paste("ED RMSE:", round(est_model_ED_test_RMSE,3),"run",i))
    
    ## ------
    ## LSTM
    ## ------
    
    print("LSTM Started")
    source("code/lstm.R")
    est_model_LSTM <- lstm(Y=Y_sub, p.weights, treat_indices, d, t0, T)
    est_model_LSTM[est_model_LSTM <0] <- 0
    est_model_LSTM <- round(est_model_LSTM)
    est_model_LSTM_msk_err <- (est_model_LSTM - Y_sub[treat_indices,][,t0:T])
    est_model_LSTM_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err^2, na.rm = TRUE))
    LSTM_RMSE_test[i] <- est_model_LSTM_test_RMSE
    print(paste("LSTM RMSE:", round(est_model_LSTM_test_RMSE,3),"run",i))
    
    ## ------
    ## VAR
    ## ------
    
    print("VAR Started")
    source("code/varEst.R")
    est_model_VAR <- varEst(Y=Y_sub, treat_indices, t0, T)
    est_model_VAR[est_model_VAR <0] <- 0
    est_model_VAR <- round(est_model_VAR)
    est_model_VAR_msk_err <- (est_model_VAR - Y_sub[treat_indices,])
    est_model_VAR_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_VAR_msk_err^2, na.rm = TRUE))
    VAR_RMSE_test[i] <- est_model_VAR_test_RMSE
    print(paste("VAR RMSE:", round(est_model_VAR_test_RMSE,3),"run",i))

    ## ------
    ## MC-NNM
    ## ------
    
    print("MC-NNM Started")
    est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 3)
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    est_model_MCPanel$Mhat[est_model_MCPanel$Mhat <0] <- 0
    est_model_MCPanel$Mhat <- round(est_model_MCPanel$Mhat)
    est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y_sub)*(1-treat_mat)
    est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
    MCPanel_RMSE_test[i] <- est_model_MCPanel$test_RMSE
    print(paste("MC-NNM RMSE:", round(est_model_MCPanel$test_RMSE,3),"run",i))
    
    ## -----
    ## DID
    ## -----
    
    print("DID Started")
    est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
    est_model_DID[est_model_DID <0] <- 0
    est_model_DID <- round(est_model_DID)
    est_model_DID_msk_err <- (est_model_DID - Y_sub)*(1-treat_mat)
    est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
    DID_RMSE_test[i] <- est_model_DID_test_RMSE
    print(paste("DID RMSE:", round(est_model_DID_test_RMSE,3),"run",i))
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
  
  ## Creating plots
  
  df1 <-
    data.frame(
      "y" =  c(DID_avg_RMSE,ED_avg_RMSE,LSTM_avg_RMSE,MCPanel_avg_RMSE,ADH_avg_RMSE,VAR_avg_RMSE),
      "lb" = c(DID_avg_RMSE - 1.96*DID_std_error,
               ED_avg_RMSE - 1.96*ED_std_error,
               LSTM_avg_RMSE - 1.96*LSTM_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error, 
               ADH_avg_RMSE - 1.96*ADH_std_error,
               VAR_avg_RMSE - 1.96*VAR_std_error),
      "ub" = c(DID_avg_RMSE + 1.96*DID_std_error, 
               ED_avg_RMSE + 1.96*ED_std_error,
               LSTM_avg_RMSE + 1.96*LSTM_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error, 
               ADH_avg_RMSE + 1.96*ADH_std_error,
               VAR_avg_RMSE + 1.96*VAR_std_error),
      "x" = c(N, N, N, N, N, N),
      "Method" = c("DID", 
                   "Encoder-decoder",
                   "LSTM", 
                   "MC-NNM", 
                   "SCM",
                   "VAR"))
  ##
  filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
  saveRDS(df1, file = paste0("results/plots/",filename))
}

# Load data
Y <- read.csv('data/sales_train_validation.csv',header=T, stringsAsFactors = F) # N X T
Y <- as.matrix(Y[,7:ncol(Y)])

print(paste0("N X T data dimension: ", dim(Y)))

for(N in c(20,50,100,200)){
  SalesSim(Y,N=N,sim=1)
}