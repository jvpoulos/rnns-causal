# Plot time-series and estimate causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-sim.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"
data.directory <-"~/Dropbox/github/rnns-causal/data/"

n.pre <- 47
n.post <- 5
output.dim <- 5

best.model <-5

analysis <- "supervised"

# Get splits

y.train <- read.csv(paste0(data.directory,"elections/sim/sim_y_train_treated.csv"), header=FALSE)

y.test <- read.csv(paste0(data.directory,"elections/sim/sim_y_test_treated.csv"), header=FALSE)

y.test.c <- y.test + abs(y.test*0.1)  #true counterfactual

# Import test results 

setwd(paste0(results.directory, "elections/sim")) # prediction files loc

test.files <- list.files(pattern = "*test.csv")

votediff.preds.test <- lapply(test.files,function(x){
  m <- read.csv(x, header=FALSE)
  return(as.matrix(m))})

votediff.preds.test.sd <- apply(simplify2array(votediff.preds.test), 1:2, sd)

#votediff.preds.test.mean <- apply(simplify2array(votediff.preds.test), 1:2, mean) # element-wise mean
votediff.preds.test.mean <- votediff.preds.test[[best.model]] # best model

# Bind predictions

votediff.bind.sim <- data.frame("y.true"=rbind(y.train,y.test[6:10,]), 
                                "y.true.c"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), y.test.c[6:10,]),
                                "y.pred"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), votediff.preds.test.mean),
                                "y.sd"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), votediff.preds.test.sd))

## Create time series data
setwd(code.directory)

## Plot time series 

votediff.bind.sim$year <- 1:nrow(votediff.bind.sim)

# Combine /take means across features

# votediff.bind.sim <- data.frame("year"=votediff.bind.year,
#                                       "y.pred"=rowMeans(votediff.bind.preds),
#                                       "y.true"=rowMeans(votediff.bind.true, na.rm = TRUE),
#                                       "y.sd"=rowMeans(votediff.bind.sds, na.rm = TRUE))
# 
# votediff.bind.sim <- votediff.bind.sim  %>%
#   mutate(pred.votediff.min = y.pred - y.sd*1.96,
#          pred.votediff.max = y.pred + y.sd*1.96,
#          pointwise.votediff = y.true - y.pred,
#          pointwise.votediff.min = y.true-pred.votediff.max,
#          pointwise.votediff.max = y.true-pred.votediff.min)

if(analysis=="supervised"){
  main <- "Encoder-decoder (+ dense output)"
  ts.plot <- TsPlotSim(votediff.bind.sim, main, n.pre, n.post)
  ggsave(paste0(results.directory,"plots/impact-sim.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="auto"){
  main <- "Encoder-decoder"
  ts.plot <- TsPlotSim(votediff.bind.sim, main, n.pre, n.post)
  ggsave(paste0(results.directory,"plots/impact-sim-auto.png"), ts.plot, width=11, height=8.5)
}

# Absolute percentage estimation error

sim.APE <- filter(votediff.bind.sim, year %in% c(48:52)) %>% mutate(APE=abs(pointwise.votediff-(y.true-y.true.c))/abs(y.true-y.true.c))