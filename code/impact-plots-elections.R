# Plot time-series and estimate causal impacts
# Uses train/test sets from elections.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-elections.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

# Import validation results (predict last 5 years of training set y using first 42 years of training set X as features)

setwd(paste0(results.directory, "elections/votediff-validation")) # prediction files loc

train.files <- list.files(pattern = "*train.csv")

votediff.preds.train <- do.call(rbind,lapply(train.files,read.csv, 
                                            header=FALSE))

votediff.preds.train <- votediff.preds.train[!duplicated(votediff.preds.train),] # 1 unique ob per sample

row.names(votediff.preds.train) <- train.files

votediff.preds.train.sd <- matrixStats::colSds(as.matrix(votediff.preds.train))

votediff.preds.train <- colMeans(as.matrix(votediff.preds.train)) # mean predictions

# Import test results (predict 5 years of test set y using all training set X as features)

setwd(paste0(results.directory, "elections/votediff-test")) # prediction files loc

test.files <- list.files(pattern = "*test.csv")

votediff.preds.test <- do.call(rbind,lapply(test.files,read.csv, 
                                       header=FALSE))

votediff.preds.test <- votediff.preds.test[!duplicated(votediff.preds.test),] # 1 unique ob per sample

row.names(votediff.preds.test) <- test.files

votediff.preds.test.sd <- matrixStats::colSds(as.matrix(votediff.preds.test))

votediff.preds.test <- colMeans(as.matrix(votediff.preds.test)) # mean predictions

# Bind predictions

votediff.bind <- data.frame(votediff.y, 
                            y.pred=c(rep(NA, 42), votediff.preds.train, votediff.preds.test),
                            y.sd=c(rep(NA, 42), votediff.preds.train.sd, votediff.preds.test.sd))

colnames(votediff.bind) <- c("year","y.true","y.pred","y.sd")

votediff.bind$pointwise.votediff <- votediff.bind$y.true - votediff.bind$y.pred

votediff.bind <- votediff.bind  %>%
  mutate(pred.votediff.min = y.pred - y.sd*1.96,
         pred.votediff.max = y.pred + y.sd*1.96,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)

# MAPE (mean absolute percentage error) on validation set
rnns.MAPE <- filter(votediff.bind, year %in% c(2000:2004)) %>% summarise(MAPE=mean(abs(y.true-y.pred)/y.true))
rnns.MAPE*100

## Create time series data
setwd(code.directory)

## Plot time series 

# ts.means.m <- melt(as.data.frame(votediff.bind[c('year','y.true', 'y.pred','pointwise.votediff')]), id.var=c("year"))

# # Adjust year for plot
votediff.bind$year <- as.Date(as.yearmon(votediff.bind$year) + 11/12, frac = 1) # end of year

votediff.bind$year <- as.POSIXct(votediff.bind$year, tz="UTC")
                         
# # Labels
# 
# ts.means.m$series <- NA
# ts.means.m$series[ts.means.m$variable=="y.true" | ts.means.m$variable=="y.pred"] <- "Winner margin time-series"
# ts.means.m$series[ts.means.m$variable=="pointwise.votediff"] <- "Pointwise impact"
# 
# ts.means.m$series<- factor(ts.means.m$series, levels=c("Winner margin time-series", "Pointwise impact")) # reverse order
# 
# levels(ts.means.m$variable) <- c("Observed votediff","Predicted votediff", 
#                                  "Pointwise votediff")
# 
# pred.vars <- c("y.pred", "y.sd", "pred.votediff.min", "pred.votediff.max", "pointwise.votediff.min", "pointwise.votediff.max")
# ts.means.m <- cbind(ts.means.m, rbind(as.matrix(votediff.bind[pred.vars]), as.matrix(votediff.bind[pred.vars]), as.matrix(votediff.bind[pred.vars])))

ts.plot <- TsPlotElections(votediff.bind)
ggsave(paste0(results.directory,"plots/impact-votediff.png"), ts.plot, width=11, height=8.5)

# Calculate avg. pointwise impact during post-period: >= 2005

# 2005
votediff.bind$pointwise.votediff[votediff.bind$year %in% c(2005)]

votediff.bind$pointwise.votediff.min[votediff.bind$year %in% c(2005)]
votediff.bind$pointwise.votediff.max[votediff.bind$year %in% c(2005)]

# 2006
votediff.bind$pointwise.votediff[votediff.bind$year %in% c(2006)]

votediff.bind$pointwise.votediff.min[votediff.bind$year %in% c(2006)]
votediff.bind$pointwise.votediff.max[votediff.bind$year %in% c(2006)]

# 2005 & 2006 (pooled)
mean(votediff.bind$pointwise.votediff[votediff.bind$year %in% c(2005,2006)])

mean(votediff.bind$pointwise.votediff.min[votediff.bind$year %in% c(2005,2006)])
mean(votediff.bind$pointwise.votediff.max[votediff.bind$year %in% c(2005,2006)])