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

california.lstm.pred.treated <- read_csv(paste0(results.directory, "lstm/california/treated/weights.5700-0.009.hdf5-california-test.csv"), col_names = FALSE)
california.lstm.pred.control <- read_csv(paste0(results.directory, "lstm/california/control/weights.7180-0.007.hdf5-california-test.csv"), col_names = FALSE)

# Actual versus predicted
california.lstm <- data.frame(
  "y.pred" = rbind(matrix(NA, california.n.pre, california.n.placebo+1), as.matrix(cbind(california.lstm.pred.treated, california.lstm.pred.control))),
  "y.true" = cbind(california.y, california.x),
  "year" =  1970:2000
)

# Post-period MSE and MAPE (all controls)

california.control.forecast <- as.matrix(california.lstm.pred.control)
california.control.true <- as.matrix(california.x[(california.n.pre+1):nrow(california.x),])

california.lstm.mse <- error(forecast=california.control.forecast, true=california.control.true, method = "mse") # post-intervention MSE

california.lstm.preds <- rbind(matrix(NA, california.n.pre, california.n.placebo+1), as.matrix(cbind(california.lstm.pred.treated, california.lstm.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

california.treat.forecast <-  as.matrix(california.lstm.pred.treated)

california.treat.true <- as.matrix(california.y[1][(california.n.pre+1):nrow(california.y),])

california.t.stat <- rowMeans(california.treat.true-california.treat.forecast) # real t stat

# P-values for both treated and placebo treated

california.p.values.treated <- PermutationTest(california.control.forecast, california.control.true, california.t.stat, california.n.placebo, np=10000)

california.p.values.control <- sapply(1:length(california.controls), function(c){
  california.t.stat.control <- rowMeans(as.matrix(california.control.true[,c])-as.matrix(california.control.forecast[,c]))
  PermutationTest(california.control.forecast[,-c], california.control.true[,-c], california.t.stat.control, california.n.placebo-1, np=10000)
})

lstm.california.fpr <- sum(california.p.values.control <=0.05)/length(california.p.values.control) #FPR

# CIs for treated

california.CI.treated <- PermutationCI(california.control.forecast, california.control.true, california.t.stat, california.n.placebo, np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
california.lstm.control <- data.frame(
  "pointwise.control" = california.x[(california.n.pre+1):nrow(california.x),]-california.control.forecast,
  "year" =  1989:2000
)

california.lstm.treat <- data.frame(
  "pointwise.treat" = california.y[(california.n.pre+1):nrow(california.y),]-california.treat.forecast, 
  "year" =  1989:2000
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
                     , legend.position = c(0.9,0.9)
                     , legend.justification = c(1,0))

california.lstm.control.long <- melt(california.lstm.control, id="year")  # convert to long format
california.lstm.control.long$group <- "Control"

california.lstm.treat.long <- melt(california.lstm.treat, id="year")  # convert to long format
california.lstm.treat.long$group <- "Treated"

california.lstm.long <- rbind(california.lstm.treat.long, california.lstm.control.long)

california.lstm.long$ymin <- NA
california.lstm.long$ymax <- NA

california.lstm.long$ymin[california.lstm.long$group=="Treated"] <- california.CI.treated[,1]
california.lstm.long$ymax[california.lstm.long$group=="Treated"] <- california.CI.treated[,2]

lstm.plot.california <- ggplot(data=california.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita cigarette sales (in packs)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("LSTM Treatment Effects: California Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/lstm-plot-effects-california.png"), lstm.plot.california, width=11, height=8.5)

# Plot p-values

california.lstm.control<- data.frame(
  "p.value" = california.p.values.control, 
  "year" =  1989:2000
)

california.lstm.treat<- data.frame(
  "p.value" = california.p.values.treated, 
  "year" =  1989:2000
)

california.lstm.control.long <- melt(california.lstm.control, id="year")  # convert to long format
california.lstm.control.long$group <- "Control"

california.lstm.treat.long <- melt(california.lstm.treat, id="year")  # convert to long format
california.lstm.treat.long$group <- "Treated"

california.lstm.long <- rbind(california.lstm.treat.long, california.lstm.control.long)

lstm.plot.pvalues.california <- ggplot(data=california.lstm.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("LSTM p-values: California Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/lstm-plot-pvalues-california.png"), lstm.plot.pvalues.california, width=11, height=8.5)

# Plot actual versus predicted

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.80)
                     , legend.justification = c(1,0))

california.lstm.plot <- ggplot(data=california.lstm, aes(x=year)) +
  geom_line(aes(y=california.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=california.lstm.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita cigarette sales (in packs))") + xlab("") +
  geom_vline(xintercept=1989, linetype=2) + 
  ggtitle(paste0("LSTM actual vs. counterfactual outcome: California Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/lstm-plot-california.png"), california.lstm.plot, width=11, height=8.5)