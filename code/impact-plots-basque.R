# Plot time-series and estimate causal impacts
# Uses train/test sets from synth-basque.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

pre.period <- 35

test.features <- 1

# Import test results

setwd(paste0(results.directory, "elections/basque")) # prediction files loc

test.files <- list.files(pattern = "*test.csv")

basque.preds.test <- lapply(test.files,function(x){
  m <- read.csv(x, header=FALSE)# get last sample
  return(as.matrix(m))})

basque.preds.test.sd <- apply(simplify2array(basque.preds.test), 1:2, sd)

#basque.preds.test.mean <- apply(simplify2array(basque.preds.test), 1:2, mean) # element-wise mean
best.model <- 14
basque.preds.test.mean <- basque.preds.test[[best.model]] # best model

# Bind predictions

basque.bind.preds <-  data.frame(y.pred=rbind(matrix(data=NA,pre.period,test.features),t(basque.preds.test.mean)))

basque.bind.sds <-  data.frame(y.sd=rbind(matrix(data=NA,pre.period,test.features),t(basque.preds.test.sd)))

basque.bind.true <-  data.frame(y.true=basque.y[colnames(basque.y) %in% colnames(basque.y.test)][-1])

## Create time series data
setwd(code.directory)

## Plot time series 

# Combine /take means across features

basque.bind <- data.frame("year"=basque.y$year,
                                "y.pred"=rowMeans(basque.bind.preds), 
                                "y.true"=rowMeans(basque.bind.true, na.rm = TRUE),
                                "y.sd"=rowMeans(basque.bind.sds, na.rm = TRUE))

basque.bind <- basque.bind  %>%
  mutate(pred.basque.min = y.pred - y.sd*1.96,
         pred.basque.max = y.pred + y.sd*1.96,
         pointwise.basque = y.true - y.pred,
         pointwise.basque.min = y.true-pred.basque.max,
         pointwise.basque.max = y.true-pred.basque.min)

# Plot actual versus predicted with credible intervals for the holdout period
basque.ed.plot <- ggplot(data=basque.bind, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1990, linetype=2) + 
  geom_ribbon(aes(ymin= pred.basque.min, ymax=pred.basque.max), fill="grey", alpha=0.5) +
  ggtitle("Basque Country: Encoder-decoder (training MSPE = 0.001; validation MSPE = 0.64)") +
  theme.blank 

ggsave(paste0(results.directory,"plots/impact-basque.png"), basque.ed.plot, width=11, height=8.5)

# Post-period MSPE

basque.ed.MSPE <- filter(basque.bind, year %in% c(1990:1997)) %>% mutate(MSPE=mean((y.true-y.pred )**2))

# Absolute percentage estimation error

basque.ed.APE <- filter(basque.bind, year %in% c(1990:1997)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))

