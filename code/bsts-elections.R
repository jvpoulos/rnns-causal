#####################################
### Causal impact estimates following Brodersen et al. ###
#####################################

library(CausalImpact)
library(dplyr)
library(tidyr)
library(tseries)
library(boot)

source(paste0(code.directory,"PolitisWhite.R"))

# Impute missing features

votediff.x.train[votediff.x.train ==-1] <- NA # revert mask to NA
votediff.x.test[votediff.x.test ==-1] <- NA # revert mask to NA

votediff.pre.train <- preProcess(votediff.x.train[!colnames(votediff.x.train) %in% c("year")], method = c("medianImpute"))
votediff.x.train[!colnames(votediff.x.train) %in% c("year")] <- predict(votediff.pre.train, votediff.x.train[!colnames(votediff.x.train) %in% c("year")] )

votediff.x.test[!colnames(votediff.x.test) %in% c("year")] <- predict(votediff.pre.train, votediff.x.test[!colnames(votediff.x.test) %in% c("year")] ) # use training values for test set 

# Take treated means

bsts.votediff.y <- votediff.y

data.votediff <- cbind(bsts.votediff.y,rbind(votediff.x.train,votediff.x.test)[-1])

data.votediff$year <-as.Date(as.yearmon(data.votediff$year) + 11/12, frac = 1) # end of year

data.votediff <- zoo(data.votediff[-1],data.votediff$year)

pre.period <- as.Date(c("1948-12-31", "2004-12-31"))
post.period <- as.Date(c("2005-12-31", "2006-12-31"))

# Regularize ts

times <- seq(start(data.votediff), end(data.votediff), by = "year")
ts.regularized <- merge(data.votediff, zoo(,times), all = TRUE)
diff(time(ts.regularized))

ts.regularized <- na.locf(ts.regularized, na.rm = FALSE) # fill missing forwards
ts.regularized <- na.locf(ts.regularized, na.rm = FALSE, fromLast=TRUE)  # fill missing backwards

# Fit model

impact.votediff <- CausalImpact(ts.regularized, 
                       pre.period, 
                       post.period, 
                       model.args = list(niter = 10000, standardize.data=TRUE)) 


summary(impact.votediff)

saveRDS(impact.votediff, paste0(data.directory,"impact.votediff.rds") ) #save

# Bootstrap estimate for prediction

votediff.bind <- cbind("year"=row.names(data.frame(impact.votediff$series)), data.frame(impact.votediff$series)[c("response","point.pred")])

votediff.bopt <- b.star(votediff.bind$point.pred,round=TRUE)[[1]]  # get optimal bootstrap lengths

GetPointwise <- function(x){ 
  # Calculate pointwise impact
  # Actual-Predicted
  return(x[,2]-x[,3])
}

votediff.boot <- tsboot(ts(votediff.bind), GetPointwise, R = 1000, l = votediff.bopt, 
                        sim = "geom") # block resampling with block lengths having a geometric distribution with mean bopt

ts.dat <- cbind(votediff.bind, data.frame(votediff.boot$t0), apply(votediff.boot$t, 2, sd))

colnames(ts.dat)[4:5] <- c("pointwise.votediff","votediff.se")

# SE

se <- ts.dat  %>%
  mutate(pred.votediff.min = point.pred - votediff.se*1.96,
         pred.votediff.max = point.pred + votediff.se*1.96,
         pointwise.votediff.min = response-pred.votediff.max,
         pointwise.votediff.max = response-pred.votediff.min,
         cumulative.votediff.min = cumsum(pointwise.votediff.min),
         cumulative.votediff.max = cumsum(pointwise.votediff.max))

se <- se[with(se, order(year)), ] # sort by year


# 2005
ts.dat$pointwise.votediff[ts.dat$year=="2005-12-31"]

se$pointwise.votediff.min[se$year=="2005-12-31"]
se$pointwise.votediff.max[se$year=="2005-12-31"]

# 2006
ts.dat$pointwise.votediff[ts.dat$year=="2006-12-31"]

se$pointwise.votediff.min[se$year=="2006-12-31"]
se$pointwise.votediff.max[se$year=="2006-12-31"]

# 2005 & 2006 (pooled)
mean(ts.dat$pointwise.votediff[ts.dat$year=="2005-12-31" | ts.dat$year=="2006-12-31"])

mean(se$pointwise.votediff.min[se$year=="2005-12-31" | se$year=="2006-12-31"])
mean(se$pointwise.votediff.max[se$year=="2005-12-31" | se$year=="2006-12-31"])

