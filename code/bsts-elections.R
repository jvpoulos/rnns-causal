#####################################
### Causal impact estimates following Brodersen et al. ###
#####################################

library(CausalImpact)
library(dplyr)
library(tidyr)

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

ts.plot.bsts <- plot(impact.votediff, c("original")) + theme(axis.text=element_text(size=12)
                                                              , axis.title.x=element_blank()
                                                              , axis.ticks.x=element_blank()
                                                              , axis.ticks.y=element_blank())

ggsave(paste0(results.directory,"plots/impact-votediff-bsts.png"), ts.plot.bsts, width=11, height=8.5)

impact.votediff$series
