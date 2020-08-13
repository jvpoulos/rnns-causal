## Reading data
SynthSim <- function(outcomes,covars.x,d,sim){
  Y <- outcomes[[d]]$M # NxT outcomes
  treat <- outcomes[[d]]$mask # NxT masked matrix 
  
  ## Setting up the configuration
  N <- nrow(treat)
  T <- ncol(treat)
  T0 <- ceiling(T*0.5)
  N_t <- ceiling(N*0.5) # no. treated units desired <=N
  num_runs <- 100
  is_simul <- sim ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  
  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
  VAR_RMSE_test <- matrix(0L,num_runs,length(T0))
  LSTM_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED_RMSE_test <- matrix(0L,num_runs,length(T0))
  DID_RMSE_test <- matrix(0L,num_runs,length(T0))
  ADH_RMSE_test <- matrix(0L,num_runs,length(T0))
  ENT_RMSE_test <- matrix(0L,num_runs,length(T0))
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    treat_indices <- sort(sample(1:N, N_t))
    for(j in c(1:length(T0))){
      t0 <- T0[j]
      ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
      if(is_simul == 1){
        treat_mat <- simul_adapt(Y, N_t, (t0-1), treat_indices) # t0 is time of initial treatment
      }else{
        treat_mat <- stag_adapt(Y, N_t, (t0-1), treat_indices)
      }
      
      Y_obs <- Y * treat_mat
      
      ## Estimate propensity scores
      
      p.mod <- cv.glmnet(x=covars.x, y=(1-treat_mat), family="mgaussian", parallel = TRUE) # LOO
      
      p.weights <- predict(p.mod, covars.x, type="response", s = "lambda.min")[,,1]
      
      ## -----
      ## ADH
      ## -----
      est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter=200, rel_tol = 0.001)
      est_model_ADH_msk_err <- (est_model_ADH - Y)*(1-treat_mat)
      est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
      ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
      print(paste("ADH RMSE:", round(est_model_ADH_test_RMSE,3),"run",i))
      
      ## ------
      ## ED
      ## ------
      
      source("code/ed.R")
      est_model_ED <- ed(Y_obs, p.weights, treat_indices, d, t0, T)
      est_model_ED_msk_err <- (est_model_ED - Y[treat_indices,][,t0:T])
      est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
      ED_RMSE_test[i,j] <- est_model_ED_test_RMSE
      print(paste("ED RMSE:", round(est_model_ED_test_RMSE,3),"run",i))
      
      ## ------
      ## LSTM
      ## ------
      
      source("code/lstm.R")
      est_model_LSTM <- lstm(Y_obs, p.weights, treat_indices, d, t0, T)
      est_model_LSTM_msk_err <- (est_model_LSTM - Y[treat_indices,][,t0:T])
      est_model_LSTM_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err^2, na.rm = TRUE))
      LSTM_RMSE_test[i,j] <- est_model_LSTM_test_RMSE
      print(paste("LSTM RMSE:", round(est_model_LSTM_test_RMSE,3),"run",i))
    
      ## ------
      ## VAR
      ## ------
      
      source("code/varEst.R")
      est_model_VAR <- varEst(Y_obs, treat_indices, t0, T)
      est_model_VAR_msk_err <- (est_model_VAR - Y[treat_indices,])
      est_model_VAR_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_VAR_msk_err^2, na.rm = TRUE))
      VAR_RMSE_test[i,j] <- est_model_VAR_test_RMSE
      print(paste("VAR RMSE:", round(est_model_VAR_test_RMSE,3),"run",i))
      
      ## ------
      ## MC-NNM
      ## ------
      
      est_model_MCPanel <- mcnnm(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, lambda_L = c(0.2), niter = 200)[[1]]
      est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
      est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y)*(1-treat_mat)
      est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
      MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE
      print(paste("MC-NNM RMSE:", round(est_model_MCPanel$test_RMSE,3),"run",i))
      
      ## -----
      ## DID
      ## -----
      
      est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
      est_model_DID_msk_err <- (est_model_DID - Y)*(1-treat_mat)
      est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
      DID_RMSE_test[i,j] <- est_model_DID_test_RMSE
      print(paste("DID RMSE:", round(est_model_DID_test_RMSE,3),"run",i))
      
      ## -----
      ## VT-EN 
      ## -----
      
      est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_folds = 3))
      est_model_ENT_msk_err <- (est_model_ENT - Y)*(1-treat_mat)
      est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2, na.rm = TRUE))
      ENT_RMSE_test[i,j] <- est_model_ENT_test_RMSE
      print(paste("VT-EN RMSE:", round(est_model_ENT_test_RMSE,3),"run",i))
    }
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
      "x" = replicate(length(T0),N*T),
      "Method" = c(replicate(length(T0),"DID"), 
                   replicate(length(T0),"Encoder-decoder"),
                   replicate(length(T0),"LSTM"), 
                   replicate(length(T0),"MC-NNM"), 
                   replicate(length(T0),"SCM"),
                   replicate(length(T0),"VAR"),
                   replicate(length(T0),"Vertical")))
  
  filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
  save(df1, file = paste0("results/plots/",filename))
}