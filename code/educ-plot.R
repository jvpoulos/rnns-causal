# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(ggplot2)
require(readr)
library(latex2exp)

# Setup parallel processing 
require(parallel)
require(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

source("code/TsPlot.R")
source("code/permutationTest.R")

PlotEduc<- function(estimator,x,y.title,limits,breaks,att.label,t0,imp,config,run.CI=TRUE, plot=TRUE){
  ## Create time series data
  
  pred.treated <- read_csv(paste0("results/", estimator,"/educ/",estimator,"-educ-test-",imp,"-",config,".csv"), col_names = FALSE)
  pred.control <- read_csv(paste0("results/", estimator,"/educ/",estimator,"-educ-train-",imp,"-",config,".csv"), col_names = FALSE)
  
  train_data <- read_csv(paste0("data/educ-x-",imp,".csv"), col_names = TRUE)
  test_data <- read_csv(paste0("data/educ-y-",imp,".csv"), col_names = TRUE)
  
  observed <- t(capacity.outcomes[[x]]$M)
  observed <- observed[,colnames(observed) %in% c(colnames(test_data),colnames(train_data))]
  observed.treated <- as.matrix(observed[,colnames(observed) %in% colnames(test_data)][(t0+1):nrow(observed),])
  observed.control <- as.matrix(observed[,colnames(observed) %in% colnames(train_data)][(t0+1):nrow(observed),])
  
  t.stat <- rowMeans(observed.treated - pred.treated) 
  
  if(run.CI){
    CI.treated <- PermutationCI(forecast=pred.control, 
                                true=observed.control, 
                                t.stat,
                                n.placebo=ncol(observed.control)-1, 
                                np=10000, 
                                l=500, 
                                prec=1e-03)
    
    saveRDS(CI.treated, paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,"-",config,".rds"))
  } else{
    CI.treated <- readRDS(paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,".rds"))
  }
  
  if(plot){
  ## Plot time series 
  
  treat.status <- matrix(colnames(observed), 
                         nrow=length(c(colnames(train_data),colnames(test_data))), ncol=1)
  treat.status[treat.status %in% colnames(test_data)] <- "Treated"
  treat.status[treat.status %in% colnames(train_data)] <- "Control"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  observed.mean <-  aggregate(t(observed), list(treat.status), mean)[-1]
  predicted.mean <-  aggregate(rbind(t(pred.control),t(pred.treated)), list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(rbind(t(observed.control),t(observed.treated))-rbind(t(pred.control),t(pred.treated)), 
                              list(treat.status), mean, na.rm=TRUE)[-1]
  
  ts.means <- cbind(t(observed.mean), rbind(matrix(NA, t0,2), t(predicted.mean)), rbind(matrix(NA, t0,2), t(pointwise.mean))) 
  colnames(ts.means) <- c("observed.sls","observed.pls","predicted.sls","predicted.pls","pointwise.sls","pointwise.pls")
  ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))
  ts.means.m <- melt(data.frame(ts.means), id.var=c("year"))
  
  CI.treated <- as.data.frame(CI.treated)
  CI.treated$year <- rownames(observed.treated)
  CI.treated$variable <- "pointwise.pls"
  colnames(CI.treated) <- c("upper","lower","year","variable")
  
  ts.means.m <- merge(ts.means.m, CI.treated, by=c("year","variable"), all.x=TRUE) # bind std. error

  # # Adjust year for plot
 # ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year
  ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year)) 
  
  ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
  
  # Labels
  
  ts.means.m$series <- NA
  ts.means.m$series[grep("observed.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("predicted.", ts.means.m$variable)] <- "Time-series"
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- att.label

  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", att.label)) # reverse order
  
  ts.means.m$hline <-NA
  ts.means.m$hline[ts.means.m$series!="Time-series"] <-0
  
  vline <- c(as.numeric(as.POSIXct("1869-1-31 00:00:00",tz="UTC")))
  
  ts.means.m$value[ts.means.m$year < vline & (ts.means.m$variable=="pointwise.pls" | ts.means.m$variable=="predicted.pls")] <- NA # censor 
  
  ts.means.m$upper[ts.means.m$year < vline & (ts.means.m$variable=="pointwise.pls" | ts.means.m$variable=="predicted.pls")] <- NA 
  
  ts.means.m$lower[ts.means.m$year < vline & ts.means.m$variable=="pointwise.pls"] <- NA 
  
  ts.plot <- TsPlot(df=ts.means.m,y.title=y.title,limits=limits, breaks=breaks,hline=ts.means.m$hline)
  
  return(ts.plot)
  } else{
    return(CI.treated)
  }
}

# for(imp in c("locf","linear","random","mean","ma")){
# 
#   capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
# 
#   # encoder-decoder
# 
#   educ.ed <- PlotEduc(estimator="encoder-decoder",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
#                            breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
#                            t0=20, imp=imp,config="tanh-128-25-0.2-0.001", run.CI=FALSE, plot=TRUE)
#   ggsave(paste0("results/plots/educ-ed-",imp,".png"), educ.ed, scale=1.25)
# 
#   # LSTM
# 
#   educ.lstm<- PlotEduc(estimator="lstm",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
#                            breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
#                            t0=20, imp=imp,config="tanh-128-25-0.2-0.001", run.CI=FALSE, plot=TRUE)
#   ggsave(paste0("results/plots/educ-lstm-",imp,".png"), educ.lstm, scale=1.25)
# }

# Compare different RNN configs
imp="locf"
capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))

# By configuration
# Hidden activation: relu, tanh, sigmoid
# No. hidden: 256, 128, 64
# Patience: 50, 25, 0
# Dropout: 0.5, 0.2, 0
#activation-nhidden-patience-dropout-L2
configs <- c(#"relu-128-25-0.2-0.001","sigmoid-128-25-0.2-0.001", "tanh-128-25-0.2-0.001",
             "tanh-256-25-0.2-0.001","relu-256-25-0.2-0.001","sigmoid-256-25-0.2-0.001",
             "tanh-64-25-0.2-0.001","relu-64-25-0.2-0.001","sigmoid-64-25-0.2-0.001",
             "tanh-128-50-0.2-0.001","relu-128-50-0.2-0.001","sigmoid-128-50-0.2-0.001",
             "tanh-256-0-0.2-0.001","relu-256-0-0.2-0.001","sigmoid-256-0-0.2-0.001",
             "tanh-128-25-0.5-0.001","relu-128-25-0.5-0.001","sigmoid-128-25-0.5-0.001",
             "tanh-256-25-0.5-0.001","relu-256-25-0.5-0.001","sigmoid-256-25-0.5-0.001",
             "tanh-64-25-0.5-0.001","relu-64-25-0.5-0.001","sigmoid-64-25-0.5-0.001",
             "tanh-128-50-0.5-0.001","relu-128-50-0.5-0.001","sigmoid-128-50-0.5-0.001",
             "tanh-256-0-0.5-0.001","relu-256-0-0.5-0.001","sigmoid-256-0-0.5-0.001",
             "tanh-128-25-0-0.001","relu-128-25-0-0.001","sigmoid-128-25-0-0.001",
             "tanh-256-25-0-0.001","relu-256-25-0-0.001","sigmoid-256-25-0-0.001",
             "tanh-64-25-0-0.001","relu-64-25-0-0.001","sigmoid-64-25-0-0.001",
             "tanh-128-50-0-0.001","relu-128-50-0-0.001","sigmoid-128-50-0-0.001",
             "tanh-256-0-0-0.001","relu-256-0-0-0.001","sigmoid-256-0-0-0.001")

for(c in configs){
  # encoder-decoder
  
  educ.ed.locf <- PlotEduc(estimator="encoder-decoder",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
                           breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
                           t0=20, imp=imp,config=c, run.CI=TRUE, plot=FALSE)
  
  # LSTM
  
  educ.ed.locf <- PlotEduc(estimator="lstm",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
                           breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
                           t0=20, imp=imp,config=c, run.CI=TRUE, plot=FALSE) 
}