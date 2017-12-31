#####################################
### Causal impact estimates with BSTS ###
# Code from http://multithreaded.stitchfix.com/blog/2016/04/21/forget-arima/ 
#####################################

library(bsts)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(reshape2)

votediff.y.bsts <- cbind(votediff.y['year'],"y.true"=rowMeans(votediff.y[-1], na.rm=TRUE)) # take treated mean

votediff.y.bsts.train <- votediff.y.bsts[votediff.y.bsts$year %in% votediff.years & votediff.y.bsts$year < 2005,]
votediff.y.bsts.test <- votediff.y.bsts[votediff.y.bsts$year %in% votediff.years & votediff.y.bsts$year >= 2005,]

#train.indices <- c(1:42)
# val.indices <- c(43:47)

### Set up the priors
# prior.val <- SpikeSlabPrior(x=model.matrix(y.true ~ ., data=cbind("y.true"=votediff.y.bsts.train$y.true[train.indices], votediff.x.train[-1][train.indices,])),
#                         y=votediff.y.bsts.train$y.true[train.indices],
#                         prior.information.weight = 0.01)
# 
prior.test <- SpikeSlabPrior(x=model.matrix(y.true ~ ., data=cbind("y.true"=votediff.y.bsts.train$y.true, votediff.x.train[-1])), 
                        y=votediff.y.bsts.train$y.true, 
                        prior.information.weight = 0.01)

## Construct state components

# ss.val <- list()
# ss.val <- AddSemilocalLinearTrend(ss.val, votediff.y.bsts.train$y.true[train.indices]) # Semilocal Linear Trend
# ss.val <- AddSeasonal(ss.val,votediff.y.bsts.train$y.true[train.indices],
#                   nseasons = 52) # monthly seasonal component

ss.test <- list()
ss.test <- AddSemilocalLinearTrend(ss.test, votediff.y.bsts.train$y.true) # Semilocal Linear Trend
ss.test <- AddSeasonal(ss.test,votediff.y.bsts.train$y.true,
                  nseasons = 52) # monthly seasonal component 

# #  Run the bsts model
# bsts.reg.val <- bsts(y.true ~ .,
#                  data = cbind("y.true"=votediff.y.bsts.train$y.true[train.indices], votediff.x.train[-1][train.indices,]),
#                  state.specification = ss.val,
#                  prior=prior.val,
#                  niter = 1000,
#                  ping = 0, seed = 2016)
# 
# saveRDS(bsts.reg.val, paste0(data.directory, "bsts-reg-val.rds"))

#bsts.reg.val <- readRDS(paste0(data.directory, "bsts-reg-val.rds"))

# bsts.reg.test <- bsts(y.true ~ .,
#                      data = cbind("y.true"=votediff.y.bsts.train$y.true, votediff.x.train[-1]),
#                      state.specification = ss.test,
#                      prior=prior.test,
#                      niter = 1000,
#                      ping = 0, seed = 2016)
# 
# saveRDS(bsts.reg.test, paste0(data.directory, "bsts-reg-test.rds"))

bsts.reg.test <- readRDS(paste0(data.directory, "bsts-reg-test.rds"))

# Get a suggested number of burn-ins to discard
# burn.val <- SuggestBurn(0.1, bsts.reg.val) 

burn.test <- SuggestBurn(0.1, bsts.reg.test) 

PositiveMean <- function(b) {
  ### Helper function to get the positive mean of a vector
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}

### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(melt(apply(bsts.reg.test$coefficients[-(1:burn.test),], 2, PositiveMean)))
coeff$Variable <- as.character(row.names(coeff))
coeff$Variable <- gsub("votediff.", "", coeff$Variable) # clean up labels
coeff$Variable <- gsub("[.]", ", ", coeff$Variable)
coeff$Variable <- gsub("([a-z])([A-Z])", "\\1 \\2", coeff$Variable)

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.9)
                     , legend.justification = c(1,0))

avg.coef.plot <- ggplot(data=coeff[coeff$value>0,], aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("Average coefficient") + ggtitle("BSTS") + theme.blank

ggsave(paste0(results.directory,"plots/bsts-coefficient-plot-test.png"), avg.coef.plot, width=11, height=8.5)

### Inclusion probabilities -- i.e., how often were the variables selected 
inclusionprobs <- melt(colMeans(bsts.reg.test$coefficients[-(1:burn.test),] != 0))
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))

inclusionprobs$Variable <- gsub("votediff.", "", inclusionprobs$Variable) # clean up labels
inclusionprobs$Variable <- gsub("[.]", ", ", inclusionprobs$Variable)
inclusionprobs$Variable <- gsub("([a-z])([A-Z])", "\\1 \\2", inclusionprobs$Variable)

inclusion.plot <- ggplot(data=inclusionprobs[inclusionprobs$value>0,], aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("Inclusion probability") + ggtitle("BSTS") + theme.blank

ggsave(paste0(results.directory,"plots/bsts-inclusion-plot-test.png"), inclusion.plot, width=11, height=8.5)

# Predict

# p.val <- predict.bsts(bsts.reg.val, 
#                   newdata = votediff.x.train[val.indices,][-1],
#                   burn = burn.val, 
#                   quantiles = c(.025, .975))

p.test <- predict.bsts(bsts.reg.test, 
                  newdata = votediff.x.test[-1],
                  burn = burn.test, 
                  quantiles = c(.025, .975))

# Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bsts.reg.test$one.step.prediction.errors[-(1:burn.test),])+votediff.y.bsts.train$y.true),  
  as.numeric(p.test$mean)), #  posterior mean of the prediction
  # actual data and dates 
  votediff.y.bsts)
names(d2) <- c("Fitted", "Date", "Actual")

# MPSE 2000 to 2004
bsts.MSPE <- filter(d2, Date %in% c(2000:2004)) %>% summarise(MSPE=mean((Actual-Fitted)**2))

# 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  as.numeric(p.test$interval[1,]),
  as.numeric(p.test$interval[2,]), 
  subset(d2, Date>=2000)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

## Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

# Plot actual versus predicted with credible intervals for the holdout period
bsts.plot <- ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Winner margin (%)") + xlab("") +
#  geom_vline(xintercept=2000, linetype=3) + 
  geom_vline(xintercept=2005, linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("Mayoral elections: BSTS (training MSPE = ", round(bsts.MSPE,2), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/bsts-plot.png"), bsts.plot, width=11, height=8.5)

# Extract the components
components <- cbind.data.frame(
  colMeans(bsts.reg.test$state.contributions[-(1:burn.test),"trend",]),                               
  colMeans(bsts.reg.test$state.contributions[-(1:burn.test),"seasonal.52.1",]),
  colMeans(bsts.reg.test$state.contributions[-(1:burn.test),"regression",]),
  votediff.y.bsts.train$year)  
names(components) <- c("Trend", "Seasonality", "Regression", "Date")
components <- melt(components, id="Date")
names(components) <- c("Date", "Component", "Value")

# Plot components
components.plot <- ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle("BSTS components") + theme.blank

ggsave(paste0(results.directory,"plots/bsts-components-plot-test.png"), components.plot, width=11, height=8.5)

## Calculate pointwise impacts

d3$pointwise <- d3$Actual-d3$Fitted
d3$pointwise.lower <- d3$Actual-(d3$Fitted+abs(d3$UL))
d3$pointwise.upper <- d3$Actual-(d3$Fitted-abs(d3$UL))