# Plot time-series and estimate causal impacts
# Uses train/test sets from elections.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-elections.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

# Import test results

setwd(paste0(results.directory, "elections/votediff")) # prediction files loc

test.files <- list.files(pattern = "*test.csv")

votediff.preds.test <- do.call(rbind,lapply(test.files,read.csv, 
                                       header=FALSE))

votediff.preds.test <- votediff.preds.test[seq(1, nrow(votediff.preds.test), 5), ] # get every 5th sample

rownames(votediff.preds.test) <- test.files

votediff.preds.test.sd <- matrixStats::colSds(as.matrix(votediff.preds.test))

votediff.preds.test <- colMeans(as.matrix(votediff.preds.test)) # mean across models

# Bind predictions

votediff.bind.elections <- data.frame(votediff.y, 
                            y.pred=c(rep(NA, 47),votediff.preds.test),
                            y.sd=c(rep(NA, 47), votediff.preds.test.sd))

colnames(votediff.bind.elections) <- c("year","y.true","y.pred","y.sd")

votediff.bind.elections$pointwise.votediff <- votediff.bind.elections$y.true - votediff.bind.elections$y.pred

votediff.bind.elections <- votediff.bind.elections  %>%
  mutate(pred.votediff.min = y.pred - y.sd*1.96,
         pred.votediff.max = y.pred + y.sd*1.96,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)

## Create time series data
setwd(code.directory)

## Plot time series 

# # Adjust year for plot
votediff.bind.elections$year <- as.Date(as.yearmon(votediff.bind.elections$year) + 11/12, frac = 1) # end of year

votediff.bind.elections$year <- as.POSIXct(votediff.bind.elections$year, tz="UTC")
                         
ts.plot <- TsPlotElections(votediff.bind.elections)
ggsave(paste0(results.directory,"plots/impact-votediff.png"), ts.plot, width=11, height=8.5)