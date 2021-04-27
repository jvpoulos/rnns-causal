# Plot time-series and causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
require(ggplot2)
options(bitmapType='cairo')
require(readr)
library(latex2exp)
library(wesanderson)

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
  
  pred.treated <- read_csv(paste0("results/", estimator,"/educ/",estimator,"-educ-test-",imp,"-",config,".csv"), col_names = FALSE, col_types = cols())
  pred.control <- read_csv(paste0("results/", estimator,"/educ/",estimator,"-educ-train-",imp,"-",config,".csv"), col_names = FALSE, col_types = cols())
  
  train_data <- read_csv(paste0("data/educ-x-",imp,".csv"), col_names = TRUE, col_types = cols())
  test_data <- read_csv(paste0("data/educ-y-",imp,".csv"), col_names = TRUE, col_types = cols())
  
  observed <- t(capacity.outcomes[[x]]$M)
  observed <- observed[,colnames(observed) %in% c(colnames(test_data),colnames(train_data))]
  observed.treated <- as.matrix(observed[,colnames(observed) %in% colnames(test_data)][(t0+1):nrow(observed),])
  observed.control <- as.matrix(observed[,colnames(observed) %in% colnames(train_data)][(t0+1):nrow(observed),])
  
  t.stat <- rowMeans(observed.treated - pred.treated) 
  names(t.stat) <- rownames(observed.treated)
  
  if(run.CI){
    CI.treated <- PermutationCI(forecast=pred.control, 
                                true=observed.control, 
                                t.stat,
                                n.placebo=ncol(observed.control)-1, 
                                np=10000, 
                                l=5000, 
                                prec=1e-03)
    
    saveRDS(CI.treated, paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,"-",config,".rds"))
  } else{
    CI.treated <- readRDS(paste0("results/", estimator,"/educ/",estimator,"-CI-treated-",imp,"-",config,".rds"))
  }
  
  rownames(CI.treated) <- rownames(observed.treated)
  
  print(paste0("Config: ", config, "; Estimator: ", estimator))
  print(paste0("ATT:", mean(t.stat[which(names(t.stat)=="1869"):which(names(t.stat)=="1942")]))) # avg over post-treatment period
  print(paste0("CI lower:", mean(CI.treated[,1][which(rownames(CI.treated)=="1869"):which(rownames(CI.treated)=="1942")])))
  print(paste0("CI upper:", mean(CI.treated[,2][which(rownames(CI.treated)=="1869"):which(rownames(CI.treated)=="1942")])))
  
  if(plot){
  ## Plot time series 
  
  observed.mean <- rbind(rowMeans(observed[,colnames(observed) %in% colnames(train_data)]), rowMeans(observed[,colnames(observed) %in% colnames(test_data)]))
  predicted.mean <-  rbind(rowMeans(pred.control),rowMeans(pred.treated))
  pointwise.mean <- rbind( rowMeans(observed.control - pred.control), rowMeans(observed.treated - pred.treated))
  
  ts.means <- cbind(t(observed.mean), rbind(matrix(NA, (t0),2), t(predicted.mean)), rbind(matrix(NA, (t0),2), t(pointwise.mean))) 
  colnames(ts.means) <- c("observed.sls","observed.pls","predicted.sls","predicted.pls","pointwise.sls","pointwise.pls")
  ts.means <- cbind(ts.means, "year"=as.numeric(rownames(ts.means)))
  ts.means.m <- melt(data.frame(ts.means), id.var=c("year"))
  
  CI.treated <- as.data.frame(CI.treated)
  CI.treated$year <- rownames(observed.treated)
  CI.treated$variable <- "pointwise.pls"
  colnames(CI.treated) <- c("upper","lower","year","variable")
  
  ts.means.m <- merge(ts.means.m, CI.treated, by=c("year","variable"), all.x=TRUE) # bind std. error

  # # Adjust year for plot
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
  
  ts.plot <- TsPlot(df=ts.means.m,y.title=y.title,limits=limits, breaks=breaks,hline=ts.means.m$hline)
  
  return(ts.plot)
  } else{
    return(CI.treated)
  }
}

activation <- c("sigmoid","tanh")
hidden <- c(256,128)
patience <- c(50,25)
dropout <- c(0.5,0.2)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(activation, hidden, patience,dropout)
colnames(hyper_grid) <- c('activation','hidden','patience','dropout')

hyper_grid$name <- paste(hyper_grid$activation, hyper_grid$hidden, hyper_grid$patience, hyper_grid$dropout, "0.01", sep="-")

for(imp in c("knn","locf","linear","ma","mean","mice","random","rf")){
  print(imp)
  
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-",imp,".rds"))
  
  if(imp!="locf"){
    config<-"tanh-128-25-0.5-0.01"
    
    # encoder-decoder
    
    educ.ed <- PlotEduc(estimator="encoder-decoder",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
                        breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
                        t0=10, imp=imp,config=config, run.CI=TRUE, plot=TRUE) # hidden_activation,n_hidden,patience,dr,penalty
    ggsave(paste0("results/plots/educ-ed-",imp,".png"), educ.ed, scale=1.25)
    
    # LSTM
    
    educ.lstm<- PlotEduc(estimator="lstm",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
                         breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
                         t0=10, imp=imp,config=config, run.CI=TRUE, plot=TRUE) # hidden_activation,n_hidden,patience,dr,penalty
    ggsave(paste0("results/plots/educ-lstm-",imp,".png"), educ.lstm, scale=1.25)
    
  }else{
    for(c in 1:nrow(hyper_grid)){
      
      config <- hyper_grid$name[c]
      
      # encoder-decoder
      
      educ.ed <- PlotEduc(estimator="encoder-decoder",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
                          breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
                          t0=10, imp=imp,config=config, run.CI=TRUE, plot=TRUE) # hidden_activation,n_hidden,patience,dr,penalty
      ggsave(paste0("results/plots/educ-ed-",imp,".png"), educ.ed, scale=1.25)
      
      # LSTM
      
      educ.lstm<- PlotEduc(estimator="lstm",x='educ.pc',y.title="Log per-capita state government education spending (1942$)\n",limits=c(as.POSIXct("1809-01-01 01:00:00"), as.POSIXct("1942-01-01 01:00:00")),
                           breaks=seq(as.POSIXct("1809-1-31 00:00:00",tz="UTC"), as.POSIXct("1942-1-31 00:00:00",tz="UTC"), "20 years"), att.label = "ATT",
                           t0=10, imp=imp,config=config, run.CI=TRUE, plot=TRUE) # hidden_activation,n_hidden,patience,dr,penalty
      ggsave(paste0("results/plots/educ-lstm-",imp,".png"), educ.lstm, scale=1.25)
    }
  }
}