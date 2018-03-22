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

patient<-TRUE

# Priors
prior.test <- SpikeSlabPrior(x=model.matrix(cigsale.23 ~ ., data=cbind(california.y.train[-1], california.x.train[-1])), 
                             y=california.y.train$cigsale.23, 
                             prior.information.weight = 0.01)

## Construct state components

ss.test <- list()
ss.test <- AddSemilocalLinearTrend(ss.test, california.y.train$cigsale.23) # Semilocal Linear Trend
ss.test <- AddSeasonal(ss.test,california.y.train$cigsale.23,
                  nseasons = 52) # monthly seasonal component 

bsts.reg.test <- bsts(cigsale.23 ~ .,
                      data = cbind(california.y.train[-1], california.x.train[-1]),
                      state.specification = ss.test,
                      prior=prior.test,
                      niter = 1000,
                      ping = 0, seed = 2016)

saveRDS(bsts.reg.test, paste0(data.directory, "bsts-reg-test-california.rds"))

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
                  newdata = california.x.test,
                  burn = burn.test, 
                  quantiles = c(.025, .975))

# Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bsts.reg.test$one.step.prediction.errors[-(1:burn.test),])+california.y.train$cigsale.23),  
  as.numeric(p.test$mean)), #  posterior mean of the prediction
  # actual data and dates 
  california.y)
names(d2) <- c("Fitted", "Date", "Actual")

# MPSE 
bsts.MSPE <- filter(d2, Date %in% c(1970:1988)) %>% summarise(MSPE=mean((Actual-Fitted)**2))
bsts.MSPE

# 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  as.numeric(p.test$interval[1,]),
  as.numeric(p.test$interval[2,]), 
  subset(d2, Date>1988)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

## Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.9)
                     , legend.justification = c(1,0))

# Plot actual versus predicted with credible intervals for the holdout period
bsts.plot <- ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1989, linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("California (placebo): BSTS (training MSPE = ", round(bsts.MSPE,3), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/bsts-plot-california.png"), bsts.plot, width=11, height=8.5)

# Post-period MSPE

california.bsts.MSPE <- filter(d3, Date %in% c(1989:2000)) %>% mutate(MSPE=mean((Actual-Fitted )**2))
california.bsts.MSPE$MSPE[1]

# Absolute percentage estimation error
california.bsts.APE <- filter(d3, Date %in% c(1989:2000)) %>% mutate(APE=abs(Fitted-Actual)/abs(Actual))
mean(california.bsts.APE$APE)