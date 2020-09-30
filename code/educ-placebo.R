######################################################################
# Education Spending Simulations #
######################################################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(softImpute)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

CapacitySim <- function(outcomes,covars.x,d,treated.indices,N,sim){
  Y <- outcomes[[d]]$M # NxT 
  Y.missing <- outcomes[[d]]$M.missing # NxT 
  treat <- outcomes[[d]]$mask # NxT masked matrix 
  
  ## Treated 
  drop.col <- sample(rownames(Y)[!rownames(Y)%in%treated.indices],1) 
  treat_y <- Y[rownames(Y)%in%c(treated.indices,drop.col),] 
  
  ## Working with the rest of matrix
  treat <- treat[!rownames(treat)%in%rownames(treat_y),]
  Y <- Y[!rownames(Y)%in%rownames(treat_y),]
  Y.missing <- Y.missing[!rownames(Y.missing)%in%rownames(treat_y),]
  covars.x <- covars.x[!rownames(covars.x)%in%c(rownames(treat_y)),]
  
  ## Setting up the configuration
  Nbig <- nrow(Y)
  N <- Nbig
  T <- ncol(treat)
  T0 <- ceiling(c(T*0.25, T*0.50, T*0.75))
  N_t <- ceiling(N*0.5) # no. treated units desired <=N
  num_runs <- 100
  is_simul <- sim ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  
  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
  VAR_RMSE_test <- matrix(0L,num_runs,length(T0))
  ED_RMSE_test <- matrix(0L,num_runs,length(T0))
  LSTM_RMSE_test <- matrix(0L,num_runs,length(T0))
  DID_RMSE_test <- matrix(0L,num_runs,length(T0))
  ADH_RMSE_test <- matrix(0L,num_runs,length(T0))
  ENT_RMSE_test <- matrix(0L,num_runs,length(T0))
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    
    ## Fix the treated units in the whole run for a better comparison
    all_indices <- sort(sample(1:Nbig, N))
    treat_indices <- sort(sample(1:N, N_t))
    Y_sub <- Y[all_indices,1:T]
    Y_sub_missing <- Y.missing[all_indices,1:T]
    covars_x_sub <- covars.x[rownames(covars.x)%in% rownames(Y_sub),]
    for (j in c(1:length(T0))){
      t0 <- T0[j]
      ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
      if(is_simul == 1){
        treat_mat <- simul_adapt(Y_sub, N_t, (t0-1), treat_indices)
      }else{
        treat_mat <- stag_adapt(Y_sub, N_t, (t0-1), treat_indices)
      }
      rownames(treat_mat) <- rownames(Y_sub)
      colnames(treat_mat) <- colnames(Y_sub)
      
      Y_obs <- Y_sub * treat_mat # treated are 0
      
      ## Estimate propensity scores
      
      p.mod <- glmnet(x=cbind(covars_x_sub,Y_obs[,1:(t0-1)]), y=(1-treat_mat), family="mgaussian", alpha=1, nlambda = 5) # avoid CV
      W <- predict(p.mod, cbind(covars_x_sub,Y_obs[,1:(t0-1)]))[,,1]
      W[,1:(t0-1)] <- W[,t0] # assume pre-treatment W same as t0
      W[W <=0.01] <- 0.01 # threshold values
      W[W >=1] <- 1-0.01 # threshold values
      
      p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
      p.weights <- treat*(1-W) + (1-treat)*(W)
      
      ## ------
      ## LSTM
      ## ------
      
      print("LSTM Started")
      source("code/lstm.R")
      est_model_LSTM <- lstm(Y_obs, p.weights, treat_indices, d, t0=20, T)
      est_model_LSTM_msk_err <- (est_model_LSTM - Y_sub)*(1-treat_mat)
      est_model_LSTM_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err^2, na.rm = TRUE))
      LSTM_RMSE_test[i,j] <- est_model_LSTM_test_RMSE
      print(paste("LSTM RMSE:", round(est_model_LSTM_test_RMSE,3),"run",i))
      
      ## ------
      ## ED
      ## ------
      
      source("code/ed.R")
      est_model_ED <- ed(Y_obs, p.weights, treat_indices, d, t0=20, T) 
      est_model_ED_msk_err <- (est_model_ED - Y_sub)*(1-treat_mat)
      est_model_ED_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ED_msk_err^2, na.rm = TRUE))
      ED_RMSE_test[i,j] <- est_model_ED_test_RMSE
      print(paste("ED RMSE:", round(est_model_ED_test_RMSE,3),"run",i))
      
      ## -----
      ## ADH
      ## -----
      print("ADH Started")
      source("code/ADH.R") # clip gradients
      est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter = 200, rel_tol = 1e-05)
      est_model_ADH_msk_err <- (est_model_ADH - Y_sub)*(1-treat_mat)
      est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
      ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
      print(paste("ADH RMSE:", round(est_model_ADH_test_RMSE,3),"run",i))
      
      ## ------
      ## VAR
      ## ------
      
      source("code/varEst.R")
      est_model_VAR <- varEst(Y_obs, treat_indices)
      est_model_VAR_msk_err <- (est_model_VAR - Y_sub)*(1-treat_mat)
      est_model_VAR_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_VAR_msk_err^2, na.rm = TRUE))
      VAR_RMSE_test[i,j] <- est_model_VAR_test_RMSE
      print(paste("VAR RMSE:", round(est_model_VAR_test_RMSE,3),"run",i))
      
      ## ------
      ## MC-NNM
      ## ------
      
      est_model_MCPanel <- mcnnm(Y_obs, treat_mat, W= p.weights, to_estimate_u = 1, to_estimate_v = 1, lambda_L = c(0.05), niter = 200, rel_tol = 1e-05)[[1]] # no CV to save computational time
      est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
      est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y_sub)*(1-treat_mat)
      est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
      MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE
      print(paste("MC-NNM RMSE:", round(est_model_MCPanel$test_RMSE,3),"run",i))
      
      ## -----
      ## DID
      ## -----
      
      est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
      est_model_DID_msk_err <- (est_model_DID - Y_sub)*(1-treat_mat)
      est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
      DID_RMSE_test[i,j] <- est_model_DID_test_RMSE
      print(paste("DID RMSE:", round(est_model_DID_test_RMSE,3),"run",i))
      
      ## -----
      ## VT-EN: : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
      ## -----
      
      est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1, num_lam = 5, num_folds = nrow(t(Y_obs)))) # avoid constant y
      est_model_ENT_msk_err <- (est_model_ENT - Y_sub)*(1-treat_mat)
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
      "y" =  c(DID_avg_RMSE,ED_avg_RMSE,LSTM_avg_RMSE,MCPanel_avg_RMSE,ADH_avg_RMSE,ENT_avg_RMSE,VAR_avg_RMSE),
      "se" = c(DID_std_error,ED_std_error,LSTM_std_error,MCPanel_std_error,ADH_std_error,ENT_std_error,VAR_std_error),
      "x" = T0/T,
      "Method" = c(replicate(length(T0),"DID"), 
                   replicate(length(T0),"Encoder-decoder"),
                   replicate(length(T0),"LSTM"),
                   replicate(length(T0),"MC-NNM"), 
                   replicate(length(T0),"SCM"),
                   replicate(length(T0),"SCM-ENT"),
                   replicate(length(T0),"VAR")))
  
  filename <- paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
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

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
treat_indices_order <- row.names(capacity.outcomes$educ.pc$M)[row.names(capacity.outcomes$educ.pc$M)%in% pub.states]

CapacitySim(outcomes=capacity.outcomes,covars.x=capacity.covars, d="educ.pc", treated.indices = treat_indices_order, sim=0)