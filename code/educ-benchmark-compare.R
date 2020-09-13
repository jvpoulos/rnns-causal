######################################################################
# Education Spending Application: Comparison estimators #
######################################################################

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

source("code/permutationTest.R")

CapacityCompare <- function(imp, drop.col, run.CI=TRUE){
  
  # Read data
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  capacity.covariates <- readRDS("data/capacity-covariates.rds")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.covariates$faval[,c("faval.1850","faval.1860")], 
                           capacity.covariates$farmsize[,c("farmsize.1790", "farmsize.1800", "farmsize.1810", "farmsize.1820", "farmsize.1830", "farmsize.1840", "farmsize.1850", "farmsize.1860")],
                           capacity.covariates$access[,c("track2.1835", "track2.1837", "track2.1839", "track2.1840", "track2.1845", "track2.1850", 
                                                         "track2.1851", "track2.1852", "track2.1854", "track2.1856", "track2.1857",
                                                         "track2.1858", "track2.1859", "track2.1860", "track2.1861")])
  
  covars.x <-capacity.covars[match(rownames(capacity.outcomes$educ.pc$M), rownames(capacity.covars)), ] # same order
  
  # Prepare outcomes data
  educ <- capacity.outcomes$educ.pc
  Y <- educ$M # NxT 
  
  Y <- Y[!rownames(Y)%in%drop.col,] #rm 1 from control group for train/test parity
  Y.missing <- educ$M.missing[!rownames(educ$M.missing)%in%drop.col,] # NxT
  covars.x <- covars.x[!rownames(covars.x)%in%drop.col,]
  
  treat <- educ$mask[!rownames(educ$mask)%in%drop.col,] # NxT masked matrix 
  t0 <- which(colnames(Y)=="1869") # first treatment time # same for all outcomes
  
  if(imp=="none"){
    Y <- Y[, - as.numeric(which(apply(Y, 2, var) == 0))] # rm col from 0 variance
    Y.missing <- Y.missing[,colnames(Y.missing) %in% colnames(Y)]
    treat <- treat[,colnames(treat) %in% colnames(Y)]
  }
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  treated.indices <- row.names(educ$M)[row.names(educ$M)%in% c("CA", "IA", "KS", "MI", "MN", "MO", "OH", "OR", "WI", "IL", "NV", "AL", "MS", "FL", "LA", "IN")]
  control.indices <-row.names(capacity.outcomes$educ.pc$M)[!row.names(capacity.outcomes$educ.pc$M)%in% c(treated.indices, drop.col)]
  
  # Censor post-period treated values
  
  treat_mat <- 1-treat # treated are 0
  
  Y_obs <- Y * treat_mat # treated are 0
  
  ## Estimate propensity scores
  
  p.mod <- cv.glmnet(x=covars.x, y=(1-treat_mat)[,T], family="binomial")
  W <- predict(p.mod, covars.x, type="response", s = "lambda.min")
  W <- replicate(T,as.vector(W)) # assume constant across T
  
  rownames(W) <- rownames(Y_obs)
  colnames(W) <- colnames(Y_obs)
  
  p.weights <- matrix(NA, nrow=nrow(W), ncol=ncol(W), dimnames = list(rownames(W), colnames(W)))
  p.weights <- (1-treat_mat) + (treat_mat)*W/(1-W) # weighting by the odds
  
  ## Compare different methods
  
  ## ------
  ## LSTM
  ## ------
  
  print("LSTM Started")
  source("code/lstm.R")
  est_model_LSTM <- lstm(Y_obs, p.weights, treat_indices, d, t0=ceiling(t0/4), T)
  est_model_LSTM_msk_err <- (Y - est_model_LSTM)*(1-treat_mat)
  LSTM_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_LSTM_msk_err)
  print(paste("LSTM ATT:", round(LSTM_test_ATT,3)))
  
  ## ------
  ## ED
  ## ------
  
  source("code/ed.R")
  est_model_ED <- ed(Y_obs, p.weights, treat_indices, d, t0=ceiling(t0/4), T) 
  est_model_ED_msk_err <- (Y - est_model_ED)*(1-treat_mat)
  ED_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_ED_msk_err)
  print(paste("ED ATT:", round(ED_test_ATT,3)))
  
  ## -----
  ## HR-EN: : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_EN <- en_mp_rows(Y_obs, treat_mat, num_alpha = 1, num_lam = 5, num_folds = nrow(t(Y_obs))) # avoid constant y
  est_model_EN_msk_err <- (Y-est_model_EN)*(1-treat_mat)
  EN_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_EN_msk_err)
  print(paste("HR-EN ATT:", round(EN_test_ATT,3)))
  
  EN_CI <- PermutationCI(forecast=t(est_model_EN[!rownames(est_model_EN)%in%treated.indices,][,t0:T]), 
                              true=t(Y[!rownames(Y)%in%treated.indices,][,t0:T]), 
                              t.stat=t(colMeans(est_model_EN_msk_err)[t0:T]),
                              n.placebo=nrow(Y[!rownames(Y)%in%treated.indices,])-1, 
                              np=100, 
                              l=250, 
                              prec=1e-03)
  
  ## -----
  ## ADH
  ## -----
  print("ADH Started")
  source("code/ADH.R") # clip gradients
  est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter = 200, rel_tol = 1e-05)
  est_model_ADH_msk_err <- (Y-est_model_ADH)*(1-treat_mat)
  ADH_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err)
  print(paste("ADH ATT:", round(ADH_test_ATT,3)))
  
  ## ------
  ## VAR
  ## ------
  
  source("code/varEst.R")
  est_model_VAR <- varEst(Y_obs, treat_indices=treated.indices)
  est_model_VAR_msk_err <- (Y-est_model_VAR)*(1-treat_mat)
  VAR_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_VAR_msk_err)
  print(paste("VAR ATT:", round(VAR_test_ATT,3)))
  
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, lambda_L = c(0.05), niter = 200, rel_tol = 1e-05)[[1]] # no CV to save computational time
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  est_model_MCPanel$msk_err <- (Y-est_model_MCPanel$Mhat)*(1-treat_mat)
  MCPanel_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err)
  print(paste("MC-NNM ATT:", round(MCPanel_test_ATT,3)))
  
  ## -----
  ## DID
  ## -----
  
  est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
  est_model_DID_msk_err <- (Y - est_model_DID)*(1-treat_mat)
  DID_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_DID_msk_err)
  print(paste("DID ATT:", round(DID_test_ATT,3)))
  
  ## -----
  ## VT-EN: : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1, num_lam = 5, num_folds = nrow(t(Y_obs)))) # avoid constant y
  est_model_ENT_msk_err <- (Y - est_model_ENT)*(1-treat_mat)
  est_model_ENT_test_ATT <- (1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err)
  print(paste("VT-EN ATT:", round(est_model_ENT_test_ATT,3)))
  
  ## Saving data
  
  df1 <-
    data.frame(
      "y" =  c(DID_test_ATT,ED_test_ATT,LSTM_test_ATT,MCPanel_test_ATT,ADH_test_ATT,EN_test_ATT,ENT_test_ATT,VAR_test_ATT),
      "CI.lower" = c(DID_CI_lower,ED_CI_lower,LSTM_CI_lower,MCPanel_CI_lower,ADH_CI_lower,EN_CI_lower,ENT_CI_lower,VAR_CI_lower),
      "CI.upper" = c(DID_CI_upper,ED_CI_upper,LSTM_CI_upper,MCPanel_CI_upper,ADH_CI_upper,EN_CI_upper,ENT_CI_upper,VAR_CI_upper),
      "Method" = c("DID", 
                   "Encoder-decoder",
                   "LSTM",
                   "MC-NNM", 
                   "SCM",
                   "SCM-EN",
                   "SCM-ENT",
                   "VAR"))
  
  filename <- paste0("educ-benchmark-compare-",imp,".rds")
  save(df1, file = paste0("results/",filename))
  
}

drop.col <-sample(c("VA","NY","VT","DE","NH","RI","CT","SC","PA","MD","NC","KY","ME","MA","NJ","TN","TX"),1) # randomly drop control unit for treat/control parity

CapacityCompare(imp="locf",drop.col)