#####################################
### lstm ### 
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

votediff.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/votediff/treated/weights.9930-3.764.hdf5-votediff-test.csv"), col_names = FALSE)
votediff.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/votediff/control/weights.300-145.691.hdf5-votediff-test.csv"), col_names = FALSE)

# Actual versus predicted
votediff.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, votediff.n.pre, votediff.n.placebo+votediff.n.treated), as.matrix(cbind(votediff.lstm.pred.treated, votediff.lstm.pred.control))),
  "y.true" = cbind(votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")], votediff.x), 
  "year" =  votediff.y.imp$year
)

# Post-period MSE and MAPE (all controls)

votediff.control.forecast <- as.matrix(votediff.lstm.pred.control)
votediff.control.true <- as.matrix(votediff.x[!colnames(votediff.x) %in% c("year")][(votediff.n.pre+1):nrow(votediff.x),])

votediff.lstm.mse <- error(forecast=votediff.control.forecast, true=votediff.control.true, method = "mse") # post-intervention MSE
votediff.lstm.mse

votediff.lstm.preds <- rbind(matrix(NA, votediff.n.pre, votediff.n.placebo+votediff.n.treated), as.matrix(cbind(votediff.lstm.pred.treated, votediff.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect ## maybe change this to imputed

votediff.treat.forecast <-  as.matrix(votediff.lstm.pred.treated)

votediff.treat.true <- as.matrix(votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")][(votediff.n.pre+1):nrow(votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")]),])

votediff.t.stat <- rowMeans(votediff.treat.true-votediff.treat.forecast, na.rm = TRUE) # real t stat
votediff.t.stat[1:2] # 2005/2006
mean(votediff.t.stat[1:2]) # pooled

(-0.001 - votediff.t.stat[1])**2 # MSPE 2005
(-0.005 - votediff.t.stat[2])**2 # MSPE 2006
(0.0005 - mean(votediff.t.stat[1:2]))**2 # MSPE pooled

# P-values for both treated and placebo treated

votediff.p.values.treated <- PermutationTest(votediff.control.forecast, votediff.control.true, votediff.t.stat, votediff.n.placebo, np=10000)

votediff.p.values.control <- sapply(1:votediff.n.placebo, function(c){
  votediff.t.stat.control <- rowMeans(as.matrix(votediff.control.true[,c])-as.matrix(votediff.control.forecast[,c]), na.rm = TRUE)
  PermutationTest(votediff.control.forecast[,-c], votediff.control.true[,-c], votediff.t.stat.control, votediff.n.placebo-votediff.n.treated, np=10000)
})

lstm.votediff.fpr <- sum(votediff.p.values.control <=0.05)/length(votediff.p.values.control) #FPR
lstm.votediff.fpr

# CIs for treated

votediff.CI.treated <- PermutationCI(votediff.control.forecast, votediff.control.true, votediff.t.stat, votediff.n.placebo, np=10000, l=500, c.range=c(-10,10))
votediff.CI.treated[1:2,]

colMeans(votediff.CI.treated[1:2,]) # pooled