#####################################
### encoder.decoder ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

germany.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/germany/treated/weights.4930-0.093.hdf5-germany-test.csv"), col_names = FALSE)
germany.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/germany/control/weights.4910-0.384.hdf5-germany-test.csv"), col_names = FALSE)

# Actual versus predicted
germany.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, germany.n.pre, germany.n.placebo+1), as.matrix(cbind(germany.encoder.decoder.pred.treated, germany.encoder.decoder.pred.control))),
  "y.true" = cbind(germany.y, germany.x),
  "year" =  1960:2003
)

# Post-period MSE and MAPE (all controls)

germany.control.forecast <- as.matrix(germany.encoder.decoder.pred.control)
germany.control.true <- as.matrix(germany.x[(germany.n.pre+1):nrow(germany.x),])

germany.encoder.decoder.mse <- error(forecast=germany.control.forecast, true=germany.control.true, method = "mse") # post-intervention MSE

germany.encoder.decoder.preds <- rbind(matrix(NA, germany.n.pre, germany.n.placebo+1), as.matrix(cbind(germany.encoder.decoder.pred.treated, germany.encoder.decoder.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

germany.treat.forecast <-  as.matrix(germany.encoder.decoder.pred.treated)

germany.treat.true <- as.matrix(germany.y[1][(germany.n.pre+1):nrow(germany.y),])

germany.t.stat <- rowMeans(germany.treat.true-germany.treat.forecast) # real t stat

# P-values for both treated and placebo treated

germany.p.values.treated <- PermutationTest(germany.control.forecast, germany.control.true, germany.t.stat, germany.n.placebo)

germany.p.values.control <- sapply(1:length(germany.controls), function(c){
  germany.t.stat.control <- rowMeans(as.matrix(germany.control.true[,c])-as.matrix(germany.control.forecast[,c]))
  PermutationTest(germany.control.forecast[,-c], germany.control.true[,-c], germany.t.stat.control, germany.n.placebo-1)
})

encoder.decoder.germany.fpr <-sum(germany.p.values.control <=0.05)/length(germany.p.values.control) #FPR

# CIs for treated

germany.CI.treated <- PermutationCI(germany.control.forecast, germany.control.true, germany.t.stat, germany.n.placebo, np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
germany.encoder.decoder.control <- data.frame(
  "pointwise.control" = germany.x[(germany.n.pre+1):nrow(germany.x),]-germany.control.forecast,
  "year" =  1990:2003
)

germany.encoder.decoder.treat <- data.frame(
  "pointwise.treat" = germany.y[(germany.n.pre+1):nrow(germany.y),]-germany.treat.forecast, 
  "year" =  1990:2003
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

germany.encoder.decoder.control.long <- melt(germany.encoder.decoder.control, id="year")  # convert to long format
germany.encoder.decoder.control.long$group <- "Control"

germany.encoder.decoder.treat.long <- melt(germany.encoder.decoder.treat, id="year")  # convert to long format
germany.encoder.decoder.treat.long$group <- "Treated"

germany.encoder.decoder.long <- rbind(germany.encoder.decoder.treat.long, germany.encoder.decoder.control.long)

germany.encoder.decoder.long$ymin <- NA
germany.encoder.decoder.long$ymax <- NA

germany.encoder.decoder.long$ymin[germany.encoder.decoder.long$group=="Treated"] <- germany.CI.treated[,1]
germany.encoder.decoder.long$ymax[germany.encoder.decoder.long$group=="Treated"] <- germany.CI.treated[,2]

encoder.decoder.plot.germany <- ggplot(data=germany.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita GDP (PPP, 2002 USD)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("Encoder-decoder Treatment Effects: West Germany Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-effects-germany.png"), encoder.decoder.plot.germany, width=11, height=8.5)

# Plot p-values

germany.encoder.decoder.control <- data.frame(
  "p.values.control" = germany.p.values.control,
  "year" =  1990:2003
)

germany.encoder.decoder.treat <- data.frame(
  "p.values.treat" = germany.p.values.treated,
  "year" =  1990:2003
)

germany.encoder.decoder.control.long <- melt(germany.encoder.decoder.control, id="year")  # convert to long format
germany.encoder.decoder.control.long$group <- "Control"

germany.encoder.decoder.treat.long <- melt(germany.encoder.decoder.treat, id="year")  # convert to long format
germany.encoder.decoder.treat.long$group <- "Treated"

germany.encoder.decoder.long <- rbind(germany.encoder.decoder.treat.long, germany.encoder.decoder.control.long)

encoder.decoder.plot.pvalues.germany <- ggplot(data=germany.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("Encoder-decoder p-values: West Germany Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-pvalues-germany.png"), encoder.decoder.plot.pvalues.germany, width=11, height=8.5)

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

germany.encoder.decoder.plot <- ggplot(data=germany.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=germany.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=germany.encoder.decoder.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita GDP (PPP, 2002 USD)") + xlab("") +
  geom_vline(xintercept=1990, linetype=2) + 
  ggtitle(paste0("Encoder-decoder actual vs. counterfactual outcome: West Germany Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-germany.png"), germany.encoder.decoder.plot, width=11, height=8.5)