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

# Get splits

x.train <- read.csv(paste0(data.directory,"elections/sim/sim_x_train_treated.csv"), header=FALSE)
x.test <- read.csv(paste0(data.directory,"elections/sim/sim_x_test_treated.csv"), header=FALSE)

y.train <- read.csv(paste0(data.directory,"elections/sim/sim_y_train_treated.csv"), header=FALSE)
y.test <- read.csv(paste0(data.directory,"elections/sim/sim_y_test_treated.csv"), header=FALSE)

phi <- -abs(y.test*0.1)

y.test.c <- y.test + phi  #true counterfactual

# Construct panel

x.sim <- cbind("id" = 1:52,
               rbind(x.train,x.test))

y.sim <- cbind("y" = rowMeans(rbind(y.train,y.test))) 

bsts.sim <- cbind(y.sim, x.sim[-1])

prior.test <- SpikeSlabPrior(x=model.matrix(y ~ ., data=bsts.sim[1:47,]), 
                        y=bsts.sim$y[1:47], 
                        prior.information.weight = 0.01)

## Construct state components

ss.test <- list()
ss.test <- AddSemilocalLinearTrend(ss.test, bsts.sim$y[1:47]) # Semilocal Linear Trend
ss.test <- AddSeasonal(ss.test,bsts.sim$y[1:47],
                  nseasons = 52) # monthly seasonal component 

# bsts.reg.test <- bsts(y ~ .,
#                      data = bsts.sim[1:47,],
#                      state.specification = ss.test,
#                      prior=prior.test,
#                      niter = 1000,
#                      ping = 0, seed = 2016)
# 
# saveRDS(bsts.reg.test, paste0(data.directory, "bsts-reg-test-sim.rds"))

bsts.reg.test <- readRDS(paste0(data.directory, "bsts-reg-test-sim.rds"))

# Get a suggested number of burn-ins to discard

burn.test <- SuggestBurn(0.1, bsts.reg.test) 

PositiveMean <- function(b) {
  ### Helper function to get the positive mean of a vector
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}
# Predict

p.test <- predict.bsts(bsts.reg.test, 
                  newdata = x.test,
                  burn = burn.test, 
                  quantiles = c(.025, .975))

# Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bsts.reg.test$one.step.prediction.errors[-(1:burn.test),])+bsts.sim$y[1:47]),  
  as.numeric(p.test$mean)), #  posterior mean of the prediction
  # actual data and dates 
  1:52,
  y.sim)
names(d2) <- c("Fitted", "Date", "Actual")

# MPSE 2000 to 2004
bsts.MSPE <- filter(d2, Date %in% c(43:47)) %>% summarise(MSPE=mean((Actual-Fitted)**2))
bsts.MSPE

# 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  as.numeric(p.test$interval[1,]),
  as.numeric(p.test$interval[2,]), 
  subset(d2, Date>47)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

## Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

# Plot actual versus predicted with credible intervals for the holdout period
bsts.plot <- ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("ARMA time-series") + xlab("Time-step") +
  geom_vline(xintercept=48, linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("Simulated data: BSTS (training MSPE = ", round(bsts.MSPE,2), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/bsts-plot-sim.png"), bsts.plot, width=11, height=8.5)

## Calculate pointwise impacts

d3$pointwise <- d3$Actual-d3$Fitted
d3$pointwise.lower <- d3$Actual-(d3$Fitted+abs(d3$UL))
d3$pointwise.upper <- d3$Actual-(d3$Fitted-abs(d3$UL))

# True Phi

d3$y.phi <- NA
d3$y.phi[d3$Date %in% c(48:52)] <- rowMeans(phi)

# Absolute percentage estimation error

bsts.sim.APE <- filter(d3, Date %in% c(48:52)) %>% mutate(APE=abs(pointwise-y.phi)/abs(y.phi))