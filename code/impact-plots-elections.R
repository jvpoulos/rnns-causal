# Plot time-series and estimate causal impacts
# Uses train/test sets from elections.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
require(tseries)
library(boot)

source(paste0(code.directory,"ts-plot-elections.R"))
source(paste0(code.directory,"PolitisWhite.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

setwd(paste0(results.directory, "elections/votediff")) # prediction files loc

# Import test results

test.files <- list.files(pattern = "*test.csv") 

votediff.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="votediff.pred"))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

votediff.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="votediff.pred"))

# Import validation fit

val.files <- list.files(pattern = "*val.csv")

votediff.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                          header=FALSE,
                                          col.names="votediff.pred"))


# Bind to splits
votediff.test <- cbind(votediff.y.test, votediff.test.pred) 

votediff.val <- cbind(votediff.y.val, votediff.val.pred) 

votediff.train <- cbind(votediff.y.train, votediff.train.pred) 

# Bootstrap estimate for prediction

votediff.bind <- rbind(rbind(votediff.train,votediff.val),votediff.test)

votediff.bopt <- b.star(votediff.bind["votediff.pred"][[1]],round=TRUE)[[1]]  # get optimal bootstrap lengths

GetPointwise <- function(x){ 
  # Calculate pointwise impact
  return(x[,2]-x[,3])
}

votediff.boot <- tsboot(ts(votediff.bind), GetPointwise, R = 1000, l = votediff.bopt, 
                        sim = "geom") # block resampling with block lengths having a geometric distribution with mean bopt


## Create time series data
setwd(code.directory)

ts.dat <- cbind(votediff.bind, data.frame(votediff.boot$t0), apply(votediff.boot$t, 2, sd))

colnames(ts.dat)[4:5] <- c("pointwise.votediff","votediff.se")

## Plot time series 

ts.means <- ts.dat  %>%
  mutate(cumulative.votediff = cumsum(pointwise.votediff))
         
ts.means <- ts.means[with(ts.means, order(year)), ] # sort by year
         
# ts.means$cumulative.votediff <- NA
#          
# for (i in 1:nrow(ts.means)){
#   ts.means$cumulative.votediff[i] <- rollmean(ts.means$pointwise.votediff,i, align='right')
# }
#          

ts.means.m <- melt(as.data.frame(ts.means)[!colnames(ts.means) %in% c("votediff.se")], id.var=c("year"))

# # Adjust year for plot
ts.means.m$year <- as.Date(as.yearmon(ts.means.m$year) + 11/12, frac = 1) # end of year

ts.means.m$year <- as.POSIXct(ts.means.m$year, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="votediff.1" | ts.means.m$variable=="votediff.pred"] <- "Winner margin time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.votediff"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.votediff"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Winner margin time-series", "Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed votediff","Predicted votediff", 
                                 "Pointwise votediff", "Cumulative votediff")

# se

se <- ts.dat  %>%
  mutate(pred.votediff.min = votediff.pred - votediff.se*1.96,
         pred.votediff.max = votediff.pred + votediff.se*1.96,
         pointwise.votediff.min = votediff.1-pred.votediff.max,
         pointwise.votediff.max = votediff.1-pred.votediff.min,
         cumulative.votediff.min = cumsum(pointwise.votediff.min),
         cumulative.votediff.max = cumsum(pointwise.votediff.max))

se <- se[with(se, order(year)), ] # sort by year

# for (i in 1:nrow(se)){
#   se$cumulative.votediff.min[i] <- rollmean(se$pointwise.votediff.min,i, align='right')
# }
# for (i in 1:nrow(se)){
#   se$cumulative.votediff.max[i] <- rollmean(se$pointwise.votediff.max,i, align='right')
# }

pred.vars <- c("votediff.pred", "votediff.se", "pred.votediff.min", "pred.votediff.max", "pointwise.votediff.min", "pointwise.votediff.max", "cumulative.votediff.min", "cumulative.votediff.max")
ts.means.m <- cbind(ts.means.m, se[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

ts.plot <- TsPlotElections(ts.means.m[ts.means.m$series != "Cumulative impact",])
ggsave(paste0(results.directory,"plots/impact-votediff.png"), ts.plot, width=11, height=8.5)

# Calculate avg. pointwise impact during post-period: >= 2005

# 2005
ts.means$pointwise.votediff[ts.means$year%in%c(2005)]

se$pointwise.votediff.min[se$year%in%c(2005)]
se$pointwise.votediff.max[se$year%in%c(2005)]

# 2006
ts.means$pointwise.votediff[ts.means$year%in%c(2006)]

se$pointwise.votediff.min[se$year%in%c(2006)]
se$pointwise.votediff.max[se$year%in%c(2006)]

# 2007
ts.means$pointwise.votediff[ts.means$year%in%c(2007)]

se$pointwise.votediff.min[se$year%in%c(2007)]
se$pointwise.votediff.max[se$year%in%c(2007)]

# 2005 & 2006 (pooled)
mean(ts.means$pointwise.votediff[ts.means$year%in%c(2005,2006)])

mean(se$pointwise.votediff.min[se$year%in%c(2005,2006)])
mean(se$pointwise.votediff.max[se$year%in%c(2005,2006)])

# 2005-2007 (pooled)
mean(ts.means$pointwise.votediff[ts.means$year%in%c(2005:2007)])

mean(se$pointwise.votediff.min[se$year%in%c(2005:2007)])
mean(se$pointwise.votediff.max[se$year%in%c(2005:2007)])