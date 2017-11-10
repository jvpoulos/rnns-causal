# Plot time-series and estimate causal impacts
# Uses train/test sets from elections.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-elections.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

setwd(paste0(results.directory, "elections/votediff")) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv")

votediff.preds <- do.call(rbind,lapply(test.files,read.csv, 
                                       header=FALSE))

votediff.preds <- votediff.preds[!duplicated(votediff.preds),]

row.names(votediff.preds) <- test.files

votediff.preds.sd <- matrixStats::colSds(as.matrix(votediff.preds))

votediff.preds <- votediff.preds[row.names(votediff.preds) %in% c("weights.12469-0.03.hdf5-votediff-test.csv"),] # keep best validated model

# Bind predictions

votediff.bind <- cbind(votediff.y.test, t(votediff.preds), votediff.preds.sd)

colnames(votediff.bind) <- c("year","y.true","y.pred","y.sd")

votediff.bind$pointwise.votediff <- votediff.bind$y.true - votediff.bind$y.pred

votediff.bind <- votediff.bind  %>%
  mutate(pred.votediff.min = y.pred - y.sd,
         pred.votediff.max = y.pred + y.sd,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)


## Create time series data
setwd(code.directory)

## Plot time series 

ts.dat <- cbind(votediff.y, "y.pred"=c(rep(NA, 47), votediff.bind$y.pred), 
                "pointwise.votediff"=c(rep(NA, 47), votediff.bind$pointwise.votediff))
#                "pred.votediff.min"=c(rep(NA, 47), votediff.bind$pred.votediff.min),
 #               "pred.votediff.max"=c(rep(NA, 47), votediff.bind$pred.votediff.max),
 #               "pointwise.votediff.min"=c(rep(NA, 47), votediff.bind$pointwise.votediff.min),
 #               "pointwise.votediff.max"=c(rep(NA, 47), votediff.bind$pointwise.votediff.max))

ts.means.m <- melt(as.data.frame(ts.dat), id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="y.true" | ts.means.m$variable=="y.pred"] <- "Winner margin time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.votediff"] <- "Pointwise impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Winner margin time-series", "Pointwise impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed votediff","Predicted votediff", 
                                 "Pointwise votediff")

pred.vars <- c("y.pred", "y.sd", "pred.votediff.min", "pred.votediff.max", "pointwise.votediff.min", "pointwise.votediff.max")
ts.means.m <- cbind(ts.means.m, rbind(matrix(data = NA, nrow = 47, ncol = length(pred.vars)), as.matrix(votediff.bind[pred.vars])))
ts.means.m[pred.vars][ts.means.m$variable=="Observed votediff",] <- NA

ts.plot <- TsPlotElections(ts.means.m)
ggsave(paste0(results.directory,"plots/impact-votediff.png"), ts.plot, width=11, height=8.5)

# Calculate avg. pointwise impact during post-period: >= 2005

# 2005
mean(votediff.bind$pointwise.votediff[votediff.bind$year %in% c(2005)])

mean(votediff.bind$pointwise.votediff.min[votediff.bind$year %in% c(2005)])
mean(votediff.bind$pointwise.votediff.max[votediff.bind$year %in% c(2005)])

# 2006
mean(votediff.bind$pointwise.votediff[votediff.bind$year %in% c(2006)])

mean(votediff.bind$pointwise.votediff.min[votediff.bind$year %in% c(2006)])
mean(votediff.bind$pointwise.votediff.max[votediff.bind$year %in% c(2006)])

# 2005 & 2006 (pooled)
mean(votediff.bind$pointwise.votediff[votediff.bind$year %in% c(2005,2006)])

mean(votediff.bind$pointwise.votediff.min[votediff.bind$year %in% c(2005,2006)])
mean(votediff.bind$pointwise.votediff.max[votediff.bind$year %in% c(2005,2006)])