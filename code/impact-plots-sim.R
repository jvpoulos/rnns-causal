# Plot time-series and estimate causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-sim.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"
data.directory <-"~/Dropbox/github/rnns-causal/data/"

n.pre <- 42
n.post <- 5

# Get splits

y.train <- read.csv(paste0(data.directory,"elections/sim/votediff_y_train_sim.csv"), header=FALSE, col.names = "y.true")

y.test <- read.csv(paste0(data.directory,"elections/sim/votediff_y_test_sim.csv"), header=FALSE)

y.test.c <- y.test + abs(y.test*0.1)  #true counterfactual

# Import test results 

setwd(paste0(results.directory, "elections/votediff-sim")) # prediction files loc

test.files <- list.files(pattern = "*test.csv")

votediff.preds.test <- do.call(rbind,lapply(test.files,read.csv, 
                                       header=FALSE))

votediff.preds.test <- votediff.preds.test[seq(1, nrow(votediff.preds.test), 5), ] # get every 5th sample

row.names(votediff.preds.test) <- test.files

votediff.preds.test.sd <- matrixStats::colSds(as.matrix(votediff.preds.test))

votediff.preds.test <- colMeans(as.matrix(votediff.preds.test)) # mean predictions

# Bind predictions

votediff.bind.sim <- data.frame("y.true"=c(y.train$y.true,y.test[[1]]), 
                            "y.true.c" =c(rep(NA, n.pre+n.post), y.test.c[[1]][6:10]),
                            "y.pred"=c(rep(NA, n.pre+n.post), votediff.preds.test),
                            "y.sd"=c(rep(NA, n.pre+n.post), votediff.preds.test.sd))

votediff.bind.sim$pointwise.votediff <- votediff.bind.sim$y.true - votediff.bind.sim$y.pred

votediff.bind.sim <- votediff.bind.sim  %>%
  mutate(pred.votediff.min = y.pred - y.sd*1.96,
         pred.votediff.max = y.pred + y.sd*1.96,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)

## Create time series data
setwd(code.directory)

## Plot time series 

# # Adjust year for plot
# votediff.bind.sim$year <- as.Date(as.yearmon(c(votediff.y.train$year,votediff.y.test$year)) + 11/12, frac = 1) # end of year
# 
# votediff.bind.sim$year <- as.POSIXct(votediff.bind.sim$year, tz="UTC")
votediff.bind.sim$year <- 1:52

ts.plot <- TsPlotSim(votediff.bind.sim)
ggsave(paste0(results.directory,"plots/impact-sim.png"), ts.plot, width=11, height=8.5)

# Absolute percentage estimation error

sim.APE <- filter(votediff.bind.sim, year %in% c(48:52)) %>% mutate(APE=abs(pointwise.votediff-(y.true-y.true.c))/abs(y.true-y.true.c))
