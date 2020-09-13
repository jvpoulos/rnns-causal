# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(ggplot2)
require(readr)

# Setup parallel processing 
require(parallel)
require(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

source("code/TsPlot.R")
source("code/permutationTest.R")

PlotEduc<- function(estimator,treated.indices,x,y.title,limits,breaks,t0,run.CI,imp){
  ## Create time series data
  
  observed <- t(capacity.outcomes[[x]]$M)[,!colnames(t(capacity.outcomes[[x]]$M)) %in% c("TN")]
  observed.treated <- as.matrix(observed[,colnames(observed) %in% treated.indices][t0:nrow(observed),])
  observed.control <- as.matrix(observed[,!colnames(observed) %in% treated.indices][t0:nrow(observed),])
  
  weights <- t(capacity.outcomes[[x]]$M)[,!colnames(t(capacity.outcomes[[x]]$M)) %in% c("TN")]
  weights.treated <- as.matrix(weights[,colnames(weights) %in% treated.indices][t0:nrow(weights),])
  weights.control <- as.matrix(weights[,!colnames(weights) %in% treated.indices][t0:nrow(weights),])
  
  pred.treated <- read_csv(paste0("results/", estimator,"/educ/",estimator,"-educ-test-",imp,"-relu-128-100-0.7-2.0.csv"), col_names = FALSE)
  pred.control <- read_csv(paste0("results/", estimator,"/educ/",estimator,"-educ-train-",imp,"-relu-128-100-0.7-2.0.csv"), col_names = FALSE)
  
  t.stat <- rowMeans(observed.treated - pred.treated) 
  
  if(run.CI){
    CI.treated <- PermutationCI(forecast=pred.control, 
                              true=observed.control, 
                              t.stat,
                              n.placebo=ncol(observed.control)-1, 
                              np=10000, 
                              l=250, 
                              prec=1e-03)
  
    saveRDS(CI.treated, paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,".rds"))
  } else{
    CI.treated <- readRDS(paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,".rds"))
  }
  
  ## Plot time series 
  
  treat.status <- matrix(colnames(observed), nrow=ncol(observed), ncol=1)
  treat.status[colnames(observed) %in% c(treated.indices)] <- "PLS"
  treat.status[!colnames(observed) %in% treated.indices] <- "SLS"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  observed.mean <-  aggregate(t(observed), list(treat.status), mean)[-1]
  predicted.mean <-  aggregate(rbind(t(pred.control),t(pred.treated)), list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(rbind(t(observed.control),t(observed.treated))-rbind(t(pred.control),t(pred.treated)), 
                              list(treat.status), mean, na.rm=TRUE)[-1]
  
  ts.means <- cbind(t(observed.mean), rbind(matrix(NA, (t0-1),2), t(predicted.mean)), rbind(matrix(NA, (t0-1),2), t(pointwise.mean))) 
  colnames(ts.means) <- c("observed.pls","observed.sls","predicted.pls","predicted.sls","pointwise.pls","pointwise.sls")
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
  ts.means.m$series[grep("pointwise.", ts.means.m$variable)] <- "Per-period impact"

  ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series", "Per-period impact")) # reverse order
  
  ts.means.m$hline <-NA
  ts.means.m$hline[ts.means.m$series!="Time-series"] <-0
  
  vline <- c(as.numeric(as.POSIXct("1869-1-31 00:00:00",tz="UTC")))
  
  ts.means.m$value[ts.means.m$year < vline & (ts.means.m$variable=="pointwise.pls" | ts.means.m$variable=="predicted.pls")] <- NA # censor 
  
  ts.means.m$upper[ts.means.m$year < vline & (ts.means.m$variable=="pointwise.pls" | ts.means.m$variable=="predicted.pls")] <- NA 
  
  ts.means.m$lower[ts.means.m$year < vline & ts.means.m$variable=="pointwise.pls"] <- NA 
  
  ts.plot <- TsPlot(df=ts.means.m,y.title=y.title,limits=limits, breaks=breaks,hline=ts.means.m$hline)
  
  return(ts.plot)
}

capacity.outcomes <- readRDS("data/capacity-outcomes-locf.rds")
p.weights <- read_csv("data/educ-wx-locf.csv")

treated.indices <- row.names(capacity.outcomes$educ.pc$M)[row.names(capacity.outcomes$educ.pc$M)%in% c("CA", "IA", "KS", "MI", "MN", "MO", "OH", "OR", "WI", "IL", "NV", "AL", "MS", "FL", "LA", "IN")]

# encoder-decoder

educ.ed.locf <- PlotEduc(estimator="encoder-decoder",treated.indices,x='educ.pc',y.title="Per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1783-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")), 
                        breaks=seq(as.POSIXct("1783-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), 
                        t0=22, run.CI=TRUE, imp="locf")
ggsave("results/plots/educ-ed-locf.png", educ.ed.locf, scale=1.25)

# LSTM

educ.ed.locf <- PlotEduc(estimator="lstm",treated.indices,x='educ.pc',y.title="Per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1783-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")), 
                         breaks=seq(as.POSIXct("1783-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), 
                         t0=22, run.CI=TRUE, imp="locf")
ggsave("results/plots/educ-lstm-locf.png", educ.ed.locf, scale=1.25)