#####################################
### encoder.decoder ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(ftsa)

votediff.n.pre <- 47
votediff.n.placebo <- 778
votediff.n.treated <- 24

# import predictions

votediff.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/votediff/treated/weights.4830-0.357.hdf5-votediff-test.csv"), col_names = FALSE)
votediff.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/votediff/control/weights.3670-0.055.hdf5-votediff-test.csv"), col_names = FALSE)

# Actual versus predicted
votediff.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, votediff.n.pre, votediff.n.placebo+votediff.n.treated), as.matrix(cbind(votediff.encoder.decoder.pred.treated, votediff.encoder.decoder.pred.control))),
  "y.true" = cbind(votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")], votediff.x), # imputed labels
  "year" =  votediff.y.imp$year
)

# Post-period MSE and MAPE (all controls)

votediff.control.forecast <- as.matrix(votediff.encoder.decoder.pred.control)
votediff.control.true <- as.matrix(votediff.x[!colnames(votediff.x) %in% c("year")][(votediff.n.pre+1):nrow(votediff.x),])

votediff.encoder.decoder.mse <- error(forecast=votediff.control.forecast, true=votediff.control.true, method = "mse") # post-intervention MSE
votediff.encoder.decoder.mse

votediff.encoder.decoder.preds <- rbind(matrix(NA, votediff.n.pre, votediff.n.placebo+votediff.n.treated), as.matrix(cbind(votediff.encoder.decoder.pred.treated, votediff.encoder.decoder.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

votediff.treat.forecast <-  as.matrix(votediff.encoder.decoder.pred.treated)

votediff.treat.true <- as.matrix(votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")][(votediff.n.pre+1):nrow(votediff.y.imp),])

votediff.t.stat <- rowMeans(votediff.treat.true-votediff.treat.forecast, na.rm = TRUE) # real t stat
votediff.t.stat[1:2] # 2005/2006
mean(votediff.t.stat[1:2]) # pooled

(-0.001 - votediff.t.stat[1])**2 # MSPE 2005
(-0.005 - votediff.t.stat[2])**2 # MSPE 2006
(0.00   lr = 0.000000105 - mean(votediff.t.stat[1:2]))**2 # MSPE pooled

# P-values for both treated and placebo treated

votediff.p.values.treated <- PermutationTest(votediff.control.forecast, votediff.control.true, votediff.t.stat, votediff.n.placebo, np=10000)

votediff.p.values.control <- sapply(1:votediff.n.placebo, function(c){
  votediff.t.stat.control <- rowMeans(as.matrix(votediff.control.true[,c])-as.matrix(votediff.control.forecast[,c]), na.rm = TRUE)
  PermutationTest(votediff.control.forecast[,-c], votediff.control.true[,-c], votediff.t.stat.control, votediff.n.placebo-votediff.n.treated, np=10000)
})

encoder.decoder.votediff.fpr <- sum(votediff.p.values.control <=0.05)/length(votediff.p.values.control) #FPR
encoder.decoder.votediff.fpr

# CIs for treated

votediff.CI.treated <- PermutationCI(votediff.control.forecast, votediff.control.true, votediff.t.stat, votediff.n.placebo, np=10000, l=1000)
votediff.CI.treated[1:2,]

colMeans(votediff.CI.treated[1:2,]) # pooled