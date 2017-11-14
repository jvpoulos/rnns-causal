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

## Impute missing features / standardize columns

votediff.x.train[votediff.x.train ==-1] <- NA # revert mask to NA
votediff.x.test[votediff.x.test ==-1] <- NA # revert mask to NA

train.indices <- c(1:44)
val.indices <- c(45:47)

votediff.pre.train <- preProcess(votediff.x.train[!colnames(votediff.x.train) %in% c("year")][train.indices,], method = c("center", "scale","medianImpute"))
votediff.x.train[!colnames(votediff.x.train) %in% c("year")][train.indices,] <- predict(votediff.pre.train, votediff.x.train[!colnames(votediff.x.train) %in% c("year")][train.indices,] )

votediff.x.train[!colnames(votediff.x.train) %in% c("year")][val.indices,] <- predict(votediff.pre.train, votediff.x.train[!colnames(votediff.x.train) %in% c("year")][val.indices,] ) # use training values for val set 

votediff.x.test[!colnames(votediff.x.test) %in% c("year")] <- predict(votediff.pre.train, votediff.x.test[!colnames(votediff.x.test) %in% c("year")] ) # use training values for test set 

### Set up the priors
prior <- SpikeSlabPrior(x=model.matrix(y.true ~ ., data=cbind("y.true"=votediff.y.train$y.true[train.indices], votediff.x.train[-1][train.indices,])), 
                        y=votediff.y.train$y.true[train.indices], 
                        prior.information.weight = 0.01)

## Construct state components

ss <- list()
ss <- AddSemilocalLinearTrend(ss, votediff.y.train$y.true[train.indices]) # Semilocal Linear Trend
ss <- AddSeasonal(ss,votediff.y.train$y.true[train.indices],
                  nseasons = 52) # monthly seasonal component 

##  Run the bsts model
bsts.reg <- bsts(y.true ~ .,
                 data = cbind("y.true"=votediff.y.train$y.true[train.indices], votediff.x.train[-1][train.indices,]), # reserve last 3 years for validation
                 state.specification = ss, 
                 prior=prior, 
                 niter = 1000,
                 ping = 0, seed = 2016) 


# Get a suggested number of burn-ins to discard
burn <- SuggestBurn(0.1, bsts.reg) 

PositiveMean <- function(b) {
  ### Helper function to get the positive mean of a vector
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}

### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(melt(apply(bsts.reg$coefficients[-(1:burn),], 2, PositiveMean)))
coeff$Variable <- as.character(row.names(coeff))
coeff$Variable <- gsub("votediff.", "", coeff$Variable) # clean up labels
coeff$Variable <- gsub("[.]", ", ", coeff$Variable)
coeff$Variable <- gsub("([a-z])([A-Z])", "\\1 \\2", coeff$Variable)

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank())

avg.coef.plot <- ggplot(data=coeff[coeff$value>0,], aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficient") + theme.blank

ggsave(paste0(results.directory,"plots/bsts-coefficient-plot.png"), avg.coef.plot, width=11, height=8.5)

### Inclusion probabilities -- i.e., how often were the variables selected 
inclusionprobs <- melt(colMeans(bsts.reg$coefficients[-(1:burn),] != 0))
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))

inclusionprobs$Variable <- gsub("votediff.", "", inclusionprobs$Variable) # clean up labels
inclusionprobs$Variable <- gsub("[.]", ", ", inclusionprobs$Variable)
inclusionprobs$Variable <- gsub("([a-z])([A-Z])", "\\1 \\2", inclusionprobs$Variable)

inclusion.plot <- ggplot(data=inclusionprobs[inclusionprobs$value>0,], aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probability") + theme.blank

ggsave(paste0(results.directory,"plots/bsts-inclusion-plot.png"), inclusion.plot, width=11, height=8.5)

# Predict
time.steps <- 5 # predict 5 time-steps in the future

p <- predict.bsts(bsts.reg, 
                  newdata = rbind(votediff.x.train[-1][val.indices,], votediff.x.test[-1]), 
                  horizon = time.steps,
                  burn = burn, 
                  quantiles = c(.025, .975))

# Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bsts.reg$one.step.prediction.errors[-(1:burn),])+votediff.y.train$y.true[train.indices]),  
    as.numeric(p$mean)),
  # actual data and dates 
  votediff.y)
names(d2) <- c("Fitted", "Date", "Actual")

# MAPE (mean absolute percentage error) on validation set
bsts.MAPE <- filter(d2, Date %in% c(2002,2003,2004)) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
bsts.MAPE*100

# 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  as.numeric(p$interval[1,]),
  as.numeric(p$interval[2,]), 
  subset(d2, Date>=2002)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

## Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

# Plot actual versus predicted with credible intervals for the holdout period
bsts.plot <- ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=2005, linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("BSTS model: Validation MAPE = ", round(100*bsts.MAPE,2), "%")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/bsts-plot.png"), bsts.plot, width=11, height=8.5)

# Extract the components
components <- cbind.data.frame(
  colMeans(bsts.reg$state.contributions[-(1:burn),"trend",]),                               
  colMeans(bsts.reg$state.contributions[-(1:burn),"seasonal.52.1",]),
  colMeans(bsts.reg$state.contributions[-(1:burn),"regression",]),
  votediff.y.train$year[train.indices])  
names(components) <- c("Trend", "Seasonality", "Regression", "Date")
components <- melt(components, id="Date")
names(components) <- c("Date", "Component", "Value")

# Plot components
components.plot <- ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + theme.blank

ggsave(paste0(results.directory,"plots/bsts-components-plot.png"), components.plot, width=11, height=8.5)

## Calculate pointwise impacts

d3$pointwise <- d3$Actual-d3$Fitted
d3$pointwise.lower <- d3$Actual-(d3$Fitted+abs(d3$UL))
d3$pointwise.upper <- d3$Actual-(d3$Fitted-abs(d3$UL))