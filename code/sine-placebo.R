######################################################################
# Experiments comparing ED trained on noisy inputs or with dropout #
######################################################################

## Loading Source files
library(ggplot2)
library(latex2exp)
library(MCPanel)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()/2

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

SineSim <- function(Y,Y.noisy,N){
  ## Setting up the configuration
  Nbig <- nrow(Y)
  Tbig <- ncol(Y)
  
  N <- N
  T <- 4900/N

  T0 <- ceiling(T/2)
  N_t <- ceiling(N/2)
  num_runs <- 5
  is_simul <- 1 ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  d <- 'sine'
  
  ## Matrices for saving RMSE values
  
  ED_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED1_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED2_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED3_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED4_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED5_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED6_RMSE_test <- matrix(0L,num_runs,length(T0))

  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0("Run number ", i," started:",N," times ",T))
    ## Fix the treated units in the whole run for a better comparison
    all_indices <- sample(1:Nbig, N)
    treat_indices <- sample(1:N, N_t)
    Y_sub <- Y[all_indices,1:T]
    Y_noisy_sub <- Y.noisy[all_indices,1:T]
    for (j in c(1:length(T0))){
      treat_mat <- matrix(1L, N, T) # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
      t0 <- T0[j]
      ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
      if(is_simul == 1){
        treat_mat <- simul_adapt(Y_sub, N_t, t0, treat_indices)
      }else{
        treat_mat <- stag_adapt(Y_sub, N_t, t0, treat_indices)
      }
      
      Y_obs <- Y_sub * treat_mat
      Y_noisy_obs <- Y_noisy_sub * treat_mat
      
      print(dim(Y_obs))
      source("ed-aug.R")
      
      ## ------
      ## BASELINE: ED (no dropout)
      ## ------
      
      print("BASELINE: ED (no dropout) Started")
      est_model_ED <- edAug(Y=Y_obs, treat_indices, d, t0, T, penalty=0, dropout=0, GS=0, GD=0, multiple=0)
      est_model_ED_msk_err <- (est_model_ED - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
      ED_RMSE_test[i,j] <- est_model_ED_test_RMSE
      
      ## ------
      ## ED + noisy inputs + real inputs (no dropout)
      ## ------
      
      print("ED + noisy inputs+real inputs (no dropout) Started")
      est_model_ED1 <- edAug(Y=Y_noisy_obs, treat_indices, d, t0, T, penalty=0, dropout=0, GS=0, GD=0, multiple=0,Y2=Y_obs)
      est_model_ED1_msk_err <- (est_model_ED1 - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED1_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED1_msk_err^2, na.rm = TRUE))
      ED1_RMSE_test[i,j] <- est_model_ED1_test_RMSE
      
      ## ------
      ## ED + noisy inputs (no dropout)
      ## ------
      
      print("ED + noisy inputs (no dropout) Started")
      est_model_ED2 <- edAug(Y=Y_noisy_obs, treat_indices, d, t0, T, penalty=0, dropout=0, GS=0, GD=0, multiple=0)
      est_model_ED2_msk_err <- (est_model_ED2 - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED2_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED2_msk_err^2, na.rm = TRUE))
      ED2_RMSE_test[i,j] <- est_model_ED2_test_RMSE
      
      ## ------
      ## ED + gaussian noise (no dropout)
      ## ------
      
      print("ED + gaussian noise (no dropout) Started")
      est_model_ED3 <- edAug(Y=Y_obs, treat_indices, d, t0, T, penalty=0, dropout=0, GS=0.1, GD=0, multiple=0)
      est_model_ED3_msk_err <- (est_model_ED3 - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED3_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED3_msk_err^2, na.rm = TRUE))
      ED3_RMSE_test[i,j] <- est_model_ED3_test_RMSE
      
      ## ------
      ## ED + gaussian dropout
      ## ------
      
      print("ED + gaussian dropout Started")
      est_model_ED4 <- edAug(Y=Y_obs, treat_indices, d, t0, T, penalty=0, dropout=0, GS=0, GD=0.5, multiple=0)
      est_model_ED4_msk_err <- (est_model_ED4 - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED4_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED4_msk_err^2, na.rm = TRUE))
      ED4_RMSE_test[i,j] <- est_model_ED4_test_RMSE
      
      ## ------
      ## ED + dropout, p=0.5
      ## ------
      
      print("ED + dropout Started")
      est_model_ED5 <- edAug(Y=Y_obs, treat_indices, d, t0, T, penalty=0, dropout=0.5, GS=0, GD=0, multiple=0)
      est_model_ED5_msk_err <- (est_model_ED5 - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED5_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED5_msk_err^2, na.rm = TRUE))
      ED5_RMSE_test[i,j] <- est_model_ED5_test_RMSE
      
      ## ------
      ## ED + regularization, lambda=0.1
      ## ------
      
      print("ED + dropout Started")
      est_model_ED6 <- edAug(Y=Y_obs, treat_indices, d, t0, T, penalty=0.1, dropout=0, GS=0, GD=0, multiple=0)
      est_model_ED6_msk_err <- (est_model_ED6 - Y_sub[treat_indices,][,(t0+1):T])
      est_model_ED6_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED6_msk_err^2, na.rm = TRUE))
      ED6_RMSE_test[i,j] <- est_model_ED6_test_RMSE
    }
  }
  
  ## Computing means and standard errors
  
  ED_avg_RMSE <- apply(ED_RMSE_test,2,mean)
  ED_std_error <- apply(ED_RMSE_test,2,sd)/sqrt(num_runs)
 
  ED1_avg_RMSE <- apply(ED1_RMSE_test,2,mean)
  ED1_std_error <- apply(ED1_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED2_avg_RMSE <- apply(ED2_RMSE_test,2,mean)
  ED2_std_error <- apply(ED2_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED3_avg_RMSE <- apply(ED3_RMSE_test,2,mean)
  ED3_std_error <- apply(ED3_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED4_avg_RMSE <- apply(ED4_RMSE_test,2,mean)
  ED4_std_error <- apply(ED4_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED5_avg_RMSE <- apply(ED5_RMSE_test,2,mean)
  ED5_std_error <- apply(ED5_RMSE_test,2,sd)/sqrt(num_runs)
  
  ED6_avg_RMSE <- apply(ED6_RMSE_test,2,mean)
  ED6_std_error <- apply(ED6_RMSE_test,2,sd)/sqrt(num_runs)
  
  ## Creating plots
  
  df1 <-
    data.frame(
      "y" =  c(ED_avg_RMSE,ED1_avg_RMSE,ED2_avg_RMSE,ED3_avg_RMSE,ED4_avg_RMSE,ED5_avg_RMSE,ED6_avg_RMSE),
      "lb" = c(ED_avg_RMSE - 1.96*ED_std_error,
               ED1_avg_RMSE - 1.96*ED1_std_error,
               ED2_avg_RMSE - 1.96*ED2_std_error,
               ED3_avg_RMSE - 1.96*ED3_std_error,
               ED4_avg_RMSE - 1.96*ED4_std_error,
               ED5_avg_RMSE - 1.96*ED5_std_error,
               ED6_avg_RMSE - 1.96*ED6_std_error),
      "ub" = c(ED_avg_RMSE + 1.96*ED_std_error,
               ED1_avg_RMSE + 1.96*ED1_std_error,
               ED2_avg_RMSE + 1.96*ED2_std_error,
               ED3_avg_RMSE + 1.96*ED3_std_error,
               ED4_avg_RMSE + 1.96*ED4_std_error,
               ED5_avg_RMSE + 1.96*ED5_std_error,
               ED6_avg_RMSE + 1.96*ED6_std_error),
      "N" = rep(N,7),
      "T" = rep(T,7),
      "Method" = c(replicate(length(T0),"Real inputs"),
                   replicate(length(T0),"Noisy inputs + real inputs"),
                   replicate(length(T0),"Noisy inputs"),
                   replicate(length(T0),"Gaussian Noise, sd=0.1"),
                   replicate(length(T0),"Gaussian Dropout, p=0.5"),
                   replicate(length(T0),"Dropout, p=0.5"),
                   replicate(length(T0),"L2 Loss, l=0.1")))
  filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
  saveRDS(df1, file = filename)
  return(df1)
}

# Load data

Y.val <- read.csv('../../RGAN/experiments/data/sine_val_real.csv',header=F) # real validation and test sine waves
Y.test <- read.csv('../../RGAN/experiments/data/sine_test_real.csv',header=F)

Y <- t(rbind(Y.val,Y.test)) # N X T
print(dim(Y))

Y.val.noisy <- read.csv('../../RGAN/experiments/data/sine_val_sample.csv', header=F) # noisy validation and test sine waves
Y.test.noisy <- read.csv('../../RGAN/experiments/data/sine_test_sample.csv', header=F)

Y.noisy <- t(rbind(Y.val.noisy,Y.test.noisy)) # N X T
print(dim(Y.noisy))

results <- foreach(N = c(10,20,50,70,100,140), .combine='rbind') %do% {
  SineSim(Y,Y.noisy,N)
}
saveRDS(results, "../results/encoder-decoder/sine/sine-placebo-results-aug.rds")