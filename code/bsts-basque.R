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

# Priors
prior.test <- SpikeSlabPrior(x=model.matrix(gdpcap.10 ~ ., data=cbind(basque.y.train[-1], basque.x.train[-1])), 
                             y=basque.y.train$gdpcap.10, 
                             prior.information.weight = 0.01)

## Construct state components

ss.test <- list()
ss.test <- AddSemilocalLinearTrend(ss.test, basque.y.train$gdpcap.10) # Semilocal Linear Trend
ss.test <- AddSeasonal(ss.test,basque.y.train$gdpcap.10,
                  nseasons = 52) # monthly seasonal component 

# bsts.reg.test <- bsts(gdpcap.10 ~ .,
#                       data = cbind(basque.y.train[-1], basque.x.train[-1]),
#                       state.specification = ss.test,
#                       prior=prior.test,
#                       niter = 1000,
#                       ping = 0, seed = 2016)
# 
# saveRDS(bsts.reg.test, paste0(data.directory, "bsts-reg-test-basque.rds"))

bsts.reg.test <- readRDS(paste0(data.directory, "bsts-reg-test-basque.rds"))

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
                  newdata = basque.x.test,
                  burn = burn.test, 
                  quantiles = c(.025, .975))

# Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bsts.reg.test$one.step.prediction.errors[-(1:burn.test),])+basque.y.train$gdpcap.10),  
  as.numeric(p.test$mean)), #  posterior mean of the prediction
  # actual data and dates 
  basque.y)
names(d2) <- c("Fitted", "Date", "Actual")

# MPSE 
bsts.MSPE <- filter(d2, Date %in% c(1960:1989)) %>% summarise(MSPE=mean((Actual-Fitted)**2))
bsts.MSPE

# 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  as.numeric(p.test$interval[1,]),
  as.numeric(p.test$interval[2,]), 
  subset(d2, Date>1989)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

## Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

# Plot actual versus predicted with credible intervals for the holdout period
bsts.plot <- ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1990, linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("Basque Country: BSTS (training MSPE = ", round(bsts.MSPE,2), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/bsts-plot-basque.png"), bsts.plot, width=11, height=8.5)

# Post-period MSPE

basque.bsts.MSPE <- filter(d3, Date %in% c(1990:1997)) %>% mutate(MSPE=mean((Actual-Fitted )**2))

# Absolute percentage estimation error

basque.bsts.APE <- filter(d3, Date %in% c(1990:1997)) %>% mutate(APE=abs(Fitted-Actual)/abs(Actual))
