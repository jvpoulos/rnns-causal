# Plot time-series and estimate causal impacts
# Uses train/test sets from elections.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

pre.period <- 47

test.features <- ncol(votediff.y.test)-1

# Import test results

setwd(paste0(results.directory, "elections/votediff")) # prediction files loc

test.files <- list.files(pattern = "*test.csv")

votediff.preds.test <- lapply(test.files,function(x){
  m <- read.csv(x, header=FALSE)
  return(as.matrix(m))})

votediff.preds.test.sd <- apply(simplify2array(votediff.preds.test), 1:2, sd)

#votediff.preds.test.mean <- apply(simplify2array(votediff.preds.test), 1:2, mean) # element-wise mean
best.model <- 81
votediff.preds.test.mean <- votediff.preds.test[[best.model]] # best model

# Bind predictions

votediff.bind.preds <-  data.frame(y.pred=rbind(matrix(data=NA,pre.period,test.features),votediff.preds.test.mean))

votediff.bind.sds <-  data.frame(y.sd=rbind(matrix(data=NA,pre.period,test.features),votediff.preds.test.sd))

votediff.bind.true <-  data.frame(y.true=votediff.y[colnames(votediff.y) %in% colnames(votediff.y.test)][-1]) # nonimputed

## Create time series data
setwd(code.directory)

## Plot time series 

# Combine /take means across features

votediff.bind.elections <- data.frame("year"=votediff.y$year,
                                "y.pred"=rowMeans(votediff.bind.preds), 
                                "y.true"=rowMeans(votediff.bind.true, na.rm = TRUE),
                                "y.sd"=rowMeans(votediff.bind.sds, na.rm = TRUE))

votediff.bind.elections <- votediff.bind.elections  %>%
  mutate(pred.votediff.min = y.pred - y.sd*1.96,
         pred.votediff.max = y.pred + y.sd*1.96,
         pointwise.votediff = y.true - y.pred,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)

# Plot actual versus predicted

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.9)
                     , legend.justification = c(1,0))

ed.plot <- ggplot(data=votediff.bind.elections[votediff.bind.elections$year >=1980,], aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Winner margin (%)") + xlab("") +
  geom_vline(xintercept=2005, linetype=2) + 
  geom_ribbon(aes(ymin=pred.votediff.min, ymax=pred.votediff.max), fill="grey", alpha=0.5) +
  ggtitle("Mayoral elections: Encoder-decoder (validation MSPE = 17.54)") +
  theme.blank 

ggsave(paste0(results.directory,"plots/impact-votediff.png"), ed.plot, width=11, height=8.5)
