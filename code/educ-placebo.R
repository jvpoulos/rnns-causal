###################################
# Education Spending Simulations #
###################################

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

cores <- 2

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

CapacitySim <- function(outcomes,covars.x,covars.z,d,sim,treated.indices){
  Y <- outcomes[[d]]$M # NxT 
  Y.missing <- outcomes[[d]]$M.missing # NxT 
  treat <- outcomes[[d]]$mask # NxT masked matrix 
  
  ## Treated 
  treat_y <- Y[rownames(Y)%in%treated.indices,] 
  
  ## Working with the rest of matrix
  treat <- treat[!rownames(treat)%in%c(rownames(treat_y),"TN"),] # (randomly) drop TN for parity
  Y <- Y[!rownames(Y)%in%c(rownames(treat_y),"TN"),] 
  Y.missing <- Y.missing[!rownames(Y.missing)%in%c(rownames(treat_y),"TN"),] 
  covars.x <- covars.x[!rownames(covars.x)%in%c(rownames(treat_y),"TN","GA"),] # GA not in outcomes
  covars.z <- covars.z[!rownames(covars.z)%in%c(rownames(treat_y),"TN","GA"),] 
  
  ## Setting up the configuration
  N <- nrow(treat)
  T <- ncol(treat)
  number_T0 <- 5
  T0 <- ceiling(T*((1:number_T0)*2-1)/(2*number_T0))
  N_t <- ceiling(N*0.5) # no. treated units desired <=N
  num_runs <- 25
  is_simul <- sim ## Whether to simulate Simultaneus Adoption or Staggered Adoption

  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
  VAR_RMSE_test <- matrix(0L,num_runs,length(T0))
  LSTM_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED_RMSE_test <- matrix(0L,num_runs,length(T0))
  ENT_RMSE_test <- matrix(0L,num_runs,length(T0))
  DID_RMSE_test <- matrix(0L,num_runs,length(T0))
  ADH_RMSE_test <- matrix(0L,num_runs,length(T0))
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    treat_indices <- sample(1:N, N_t)
    for (j in c(1:length(T0))){
      t0 <- T0[j]
      ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
      if(is_simul == 1){
        treat_mat <- simul_adapt(Y, N_t, (t0-1), treat_indices)
      }else{
        treat_mat <- stag_adapt(Y, N_t, (t0-1), treat_indices)
      }

      Y_obs <- Y * treat_mat
      Y_imp <- Y * Y.missing
  
      ## Estimate propensity scores
      
      logitMod.x <- cv.glmnet(x=covars.x, y=as.factor((1-treat_mat)[,t0]), family="binomial", nfolds= nrow(covars.x), parallel = TRUE) # LOO
      
      logitMod.z <- cv.glmnet(x=covars.z, y=as.factor((1-treat_mat)[treat_indices[1],]), family="binomial", nfolds=nrow(covars.z), parallel = TRUE)
      
      p.weights.x <- as.vector(predict(logitMod.x, covars.x, type="response", s ="lambda.min"))
      p.weights.z <- as.vector(predict(logitMod.z, covars.z, type="response", s ="lambda.min"))
      
      p.weights <- outer(p.weights.x,p.weights.z)   # outer product of fitted values on response scale
  
      ## ------
      ## LSTM
      ## ------
      
      source("code/lstm.R")
      est_model_LSTM <- lstm(Y, p.weights, treat_indices, d, t0, T)
      est_model_LSTM_msk_err <- (est_model_LSTM - Y_imp[treat_indices,][,t0:T])
      est_model_LSTM_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err^2, na.rm = TRUE))
      LSTM_RMSE_test[i,j] <- est_model_LSTM_test_RMSE
      print(paste("LSTM RMSE:", round(est_model_LSTM_test_RMSE,3),"run",i))
      
      ## ------
      ## ED
      ## ------
      
      source("code/ed.R")
      est_model_ED <- ed(Y, p.weights, treat_indices, d, t0, T)
      est_model_ED_msk_err <- (est_model_ED - Y_imp[treat_indices,][,t0:T])
      est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
      ED_RMSE_test[i,j] <- est_model_ED_test_RMSE
      print(paste("ED RMSE:", round(est_model_ED_test_RMSE,3),"run",i))
      
      ## ------
      ## VAR
      ## ------
      
      source("code/varEst.R")
      est_model_VAR <- varEst(Y, treat_indices, t0, T)
      est_model_VAR_msk_err <- (est_model_VAR - Y_imp[treat_indices,][,t0:T])
      est_model_VAR_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_VAR_msk_err^2, na.rm = TRUE))
      VAR_RMSE_test[i,j] <- est_model_VAR_test_RMSE
      print(paste("VAR RMSE:", round(est_model_VAR_test_RMSE,3),"run",i))
      
      ## ------
      ## MC-NNM
      ## ------
      
      est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 2)
      est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
      est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y_imp)*(1-treat_mat)
      est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
      MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE
      print(paste("MC-NNM RMSE:", round(est_model_MCPanel$test_RMSE,3),"run",i))
      
      ## -----
      ## VT-EN 
      ## -----
      
      est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat)))
      est_model_ENT_msk_err <- (est_model_ENT - Y_imp)*(1-treat_mat)
      est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2, na.rm = TRUE))
      ENT_RMSE_test[i,j] <- est_model_ENT_test_RMSE
      print(paste("VT-EN RMSE:", round(est_model_ENT_test_RMSE,3),"run",i))
      
      ## -----
      ## DID
      ## -----
      
      est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
      est_model_DID_msk_err <- (est_model_DID - Y_imp)*(1-treat_mat)
      est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
      DID_RMSE_test[i,j] <- est_model_DID_test_RMSE
      print(paste("DID RMSE:", round(est_model_DID_test_RMSE,3),"run",i))
      
      ## -----
      ## ADH
      ## -----
      est_model_ADH <- adh_mp_rows(Y_obs, treat_mat)
      est_model_ADH_msk_err <- (est_model_ADH - Y_imp)*(1-treat_mat)
      est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
      ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
      print(paste("ADH RMSE:", round(est_model_ADH_test_RMSE,3),"run",i))
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
  
  ENT_avg_RMSE <- apply(ENT_RMSE_test,2,mean)
  ENT_std_error <- apply(ENT_RMSE_test,2,sd)/sqrt(num_runs)
  
  DID_avg_RMSE <- apply(DID_RMSE_test,2,mean)
  DID_std_error <- apply(DID_RMSE_test,2,sd)/sqrt(num_runs)
  
  ADH_avg_RMSE <- apply(ADH_RMSE_test,2,mean)
  ADH_std_error <- apply(ADH_RMSE_test,2,sd)/sqrt(num_runs)
  
  ## Creating plots
  
  df1 <-
    data.frame(
      "y" =  c(DID_avg_RMSE,ED_avg_RMSE,LSTM_avg_RMSE,MCPanel_avg_RMSE,ADH_avg_RMSE,ENT_avg_RMSE,VAR_avg_RMSE),
      "lb" = c(DID_avg_RMSE - 1.96*DID_std_error,
               ED_avg_RMSE - 1.96*ED_std_error,
               LSTM_avg_RMSE - 1.96*LSTM_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error, 
               ADH_avg_RMSE - 1.96*ADH_std_error,
               ENT_avg_RMSE - 1.96*ENT_std_error,
               VAR_avg_RMSE - 1.96*VAR_std_error),
      "ub" = c(DID_avg_RMSE + 1.96*DID_std_error, 
               ED_avg_RMSE + 1.96*ED_std_error,
               LSTM_avg_RMSE + 1.96*LSTM_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error, 
               ADH_avg_RMSE + 1.96*ADH_std_error,
               ENT_avg_RMSE + 1.96*ENT_std_error,
               VAR_avg_RMSE + 1.96*VAR_std_error),
      "x" = c(T0/T, T0/T ,T0/T, T0/T, T0/T, T0/T, T0/T),
      "Method" = c(replicate(length(T0),"DID"), 
                   replicate(length(T0),"Encoder-decoder"),
                   replicate(length(T0),"LSTM"), 
                   replicate(length(T0),"MC-NNM"), 
                   replicate(length(T0),"SCM"),
                   replicate(length(T0),"SCM-EN"),
                   replicate(length(T0),"VAR")))
  
  filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
  save(df1, file = paste0("results/plots/",filename))

}

# Read data
capacity.outcomes <- readRDS("data/capacity-outcomes-locf.rds")
capacity.covariates <- readRDS("data/capacity-covariates.rds")

# Transform covars to unit and time-specific inputs
capacity.covars.x <- as.matrix(bind_rows(lapply(capacity.covariates,rowMeans))) # N x # predictors
rownames(capacity.covars.x) <- rownames(capacity.covariates$faval)

# matrix of same time dimension as outcomes
capacity.covars.z <- list("faval"=matrix(NA, nrow = nrow(capacity.outcomes$educ.pc$M), ncol = ncol(capacity.outcomes$educ.pc$M)),
                          "farmsize"=matrix(NA, nrow = nrow(capacity.outcomes$educ.pc$M), ncol = ncol(capacity.outcomes$educ.pc$M)),
                          "access"=matrix(NA, nrow = nrow(capacity.outcomes$educ.pc$M), ncol = ncol(capacity.outcomes$educ.pc$M)))

for(i in c("faval","farmsize","access")){
  colnames(capacity.covars.z[[i]]) <- colnames(capacity.outcomes$educ.pc$M)
  rownames(capacity.covars.z[[i]]) <- rownames(capacity.outcomes$educ.pc$M)
}

colnames(capacity.covariates$faval) <- sub('faval.', '', colnames(capacity.covariates$faval))
colnames(capacity.covariates$farmsize) <- sub('farmsize.', '', colnames(capacity.covariates$farmsize))
colnames(capacity.covariates$access) <- sub('track2.', '', colnames(capacity.covariates$access))

# fill in observed data and impute missing 

for(i in c("faval","farmsize","access")){
  for(n in rownames(capacity.outcomes$educ.pc$M)){
    for(t in colnames(capacity.outcomes$educ.pc$M)){
      capacity.covars.z[[i]][n,][t] <- capacity.covariates[[i]][n,][t]
    }
  }
  
  capacity.covars.z[[i]] <- t(capacity.covars.z[[i]])
  preProcValues <- preProcess(capacity.covars.z[[i]], method = c("medianImpute"), verbose=TRUE) # use training set median
  
  capacity.covars.z[[i]] <- na_locf(capacity.covars.z[[i]], option = "locf", na_remaining = "keep") 
  
  capacity.covars.z[[i]] <- predict(preProcValues, capacity.covars.z[[i]])
  
  capacity.covars.z[[i]] <- t(capacity.covars.z[[i]])
}

capacity.covars.z <- as.matrix(bind_rows(lapply(capacity.covars.z,colMeans))) # T x # predictors
rownames(capacity.covars.z) <- colnames(capacity.outcomes$educ.pc$M)

treat_indices_order <- c("CA", "IA", "KS", "MI", "MN", "MO", "OH", "OR", "WI", "IL", "NV", "AL", "MS", "FL", "LA", "IN")

CapacitySim(outcomes=capacity.outcomes,covars.x=capacity.covars.x, covars.z= capacity.covars.z, d="educ.pc",sim=1,treated.indices = treat_indices_order)