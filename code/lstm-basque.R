#####################################
### lstm ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

basque.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/basque/treated/weights.9910-0.002.hdf5-basque-test.csv"), col_names = FALSE)
basque.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/basque/control/weights.1140-0.002.hdf5-basque-test.csv"), col_names = FALSE)

# Actual versus predicted
basque.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, basque.n.pre, basque.n.placebo+1), as.matrix(cbind(basque.lstm.pred.treated, basque.lstm.pred.control))),
  "y.true" = cbind(basque.y, basque.x),
  "year" =  1955:1997
)

# Post-period MSE and MAPE (all controls)

basque.control.forecast <- as.matrix(basque.lstm.pred.control)
basque.control.true <- as.matrix(basque.x[(basque.n.pre+1):nrow(basque.x),])

basque.lstm.mse <- error(forecast=basque.control.forecast, true=basque.control.true, method = "mse") # post-intervention MSE

basque.lstm.preds <- rbind(matrix(NA, basque.n.pre, basque.n.placebo+1), as.matrix(cbind(basque.lstm.pred.treated, basque.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

basque.treat.forecast <-  as.matrix(basque.lstm.pred.treated)

basque.treat.true <- as.matrix(basque.y[1][(basque.n.pre+1):nrow(basque.y),])

basque.t.stat <- rowMeans(basque.treat.true-basque.treat.forecast) # real t stat

# P-values for both treated and placebo treated

basque.p.values.treated <- PermutationTest(basque.control.forecast, basque.control.true, basque.t.stat, basque.n.placebo)

basque.p.values.control <- sapply(1:length(basque.controls), function(c){
  basque.t.stat.control <- rowMeans(as.matrix(basque.control.true[,c])-as.matrix(basque.control.forecast[,c]))
  PermutationTest(basque.control.forecast[,-c], basque.control.true[,-c], basque.t.stat.control, basque.n.placebo-1)
})

lstm.basque.fpr <- sum(basque.p.values.control <=0.05)/length(basque.p.values.control) #FPR

# CIs for treated

basque.CI.treated <- PermutationCI(basque.control.forecast, basque.control.true, basque.t.stat, basque.n.placebo, np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
basque.lstm.control <- data.frame(
  "pointwise.control" = basque.x[(basque.n.pre+1):nrow(basque.x),]-basque.control.forecast,
  "year" =  1969:1997
)

basque.lstm.treat <- data.frame(
  "pointwise.treat" = basque.y[(basque.n.pre+1):nrow(basque.y),]-basque.treat.forecast, 
  "year" =  1969:1997
)

theme.blank <- theme(axis.text=element_text(size=14)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5, size=16)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , panel.grid.major = element_blank()
                     , panel.grid.minor = element_blank()
                     , legend.text=element_text(size=14)
                     , legend.title = element_blank()
                     , legend.position = c(0.2,0.9)
                     , legend.justification = c(1,0))

basque.lstm.control.long <- melt(basque.lstm.control, id="year")  # convert to long format
basque.lstm.control.long$group <- "Control"

basque.lstm.treat.long <- melt(basque.lstm.treat, id="year")  # convert to long format
basque.lstm.treat.long$group <- "Treated"

basque.lstm.long <- rbind(basque.lstm.treat.long, basque.lstm.control.long)

basque.lstm.long$ymin <- NA
basque.lstm.long$ymax <- NA

basque.lstm.long$ymin[basque.lstm.long$group=="Treated"] <- basque.CI.treated[,1]
basque.lstm.long$ymax[basque.lstm.long$group=="Treated"] <- basque.CI.treated[,2]

lstm.plot.basque <- ggplot(data=basque.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log real per-capita GDP (1986 USD, thousand)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("LSTM Treatment Effects: Basque Country Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/lstm-plot-effects-basque.png"), lstm.plot.basque, width=11, height=8.5)

# Plot p-values

basque.lstm.control <- data.frame(
  "p.values.control" = basque.p.values.control,
  "year" =  1969:1997
)

basque.lstm.treat <- data.frame(
  "p.values.treat" = basque.p.values.treated,
  "year" =  1969:1997
)

basque.lstm.control.long <- melt(basque.lstm.control, id="year")  # convert to long format
basque.lstm.control.long$group <- "Control"

basque.lstm.treat.long <- melt(basque.lstm.treat, id="year")  # convert to long format
basque.lstm.treat.long$group <- "Treated"

basque.lstm.long <- rbind(basque.lstm.treat.long, basque.lstm.control.long)

lstm.plot.pvalues.basque <- ggplot(data=basque.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("LSTM p-values: Basque Country Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/lstm-plot-pvalues-basque.png"), lstm.plot.pvalues.basque, width=11, height=8.5)

# Plot actual versus predicted with credible intervals for the holdout period

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.85)
                     , legend.justification = c(1,0))

basque.lstm.plot <- ggplot(data=basque.lstm, aes(x=year)) +
  geom_line(aes(y=basque.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=basque.lstm.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1969, linetype=2) + 
  ggtitle(paste0("LSTM actual vs. counterfactual outcome: Basque Country Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/lstm-plot-basque.png"), basque.lstm.plot, width=11, height=8.5)