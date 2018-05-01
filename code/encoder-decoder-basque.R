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

basque.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/basque/treated/weights.350-0.022.hdf5-basque-test.csv"), col_names = FALSE)
basque.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/basque/control/weights.460-0.071.hdf5-basque-test.csv"), col_names = FALSE)

# Actual versus predicted
basque.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, basque.n.pre, basque.n.placebo+1), as.matrix(cbind(basque.encoder.decoder.pred.treated, basque.encoder.decoder.pred.control))),
  "y.true" = cbind(basque.y, basque.x),
  "year" =  1955:1997
)

# Post-period MSE and MAPE (all controls)

basque.control.forecast <- as.matrix(basque.encoder.decoder.pred.control)
basque.control.true <- as.matrix(basque.x[(basque.n.pre+1):nrow(basque.x),])

basque.encoder.decoder.mse <- error(forecast=basque.control.forecast, true=basque.control.true, method = "mse") # post-intervention MSE

basque.encoder.decoder.preds <- rbind(matrix(NA, basque.n.pre, basque.n.placebo+1), as.matrix(cbind(basque.encoder.decoder.pred.treated, basque.encoder.decoder.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

basque.treat.forecast <-  as.matrix(basque.encoder.decoder.pred.treated)

basque.treat.true <- as.matrix(basque.y[1][(basque.n.pre+1):nrow(basque.y),])

basque.t.stat <- rowMeans(basque.treat.true-basque.treat.forecast) # real t stat

# P-values for both treated and placebo treated

basque.p.values.treated <- PermutationTest(basque.control.forecast, basque.control.true, basque.t.stat, basque.n.placebo)

basque.p.values.control <- sapply(1:length(basque.controls), function(c){
  basque.t.stat.control <- rowMeans(as.matrix(basque.control.true[,c])-as.matrix(basque.control.forecast[,c]))
  PermutationTest(basque.control.forecast[,-c], basque.control.true[,-c], basque.t.stat.control, basque.n.placebo-1)
})

encoder.decoder.basque.fpr <- sum(basque.p.values.control <=0.05)/length(basque.p.values.control) #FPR

# CIs for treated

basque.CI.treated <- PermutationCI(basque.control.forecast, basque.control.true, basque.t.stat, basque.n.placebo, np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
basque.encoder.decoder.control <- data.frame(
  "pointwise.control" = basque.x[(basque.n.pre+1):nrow(basque.x),]-basque.control.forecast,
  "year" =  1969:1997
)

basque.encoder.decoder.treat <- data.frame(
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

basque.encoder.decoder.control.long <- melt(basque.encoder.decoder.control, id="year")  # convert to long format
basque.encoder.decoder.control.long$group <- "Control"

basque.encoder.decoder.treat.long <- melt(basque.encoder.decoder.treat, id="year")  # convert to long format
basque.encoder.decoder.treat.long$group <- "Treated"

basque.encoder.decoder.long <- rbind(basque.encoder.decoder.treat.long, basque.encoder.decoder.control.long)

basque.encoder.decoder.long$ymin <- NA
basque.encoder.decoder.long$ymax <- NA

basque.encoder.decoder.long$ymin[basque.encoder.decoder.long$group=="Treated"] <- basque.CI.treated[,1]
basque.encoder.decoder.long$ymax[basque.encoder.decoder.long$group=="Treated"] <- basque.CI.treated[,2]

encoder.decoder.plot.basque <- ggplot(data=basque.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log real per-capita GDP (1986 USD, thousand)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("Encoder-decoder Treatment Effects: Basque Country Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-effects-basque.png"), encoder.decoder.plot.basque, width=11, height=8.5)

# Plot p-values

basque.encoder.decoder.control <- data.frame(
  "p.values.control" = basque.p.values.control,
  "year" =  1969:1997
)

basque.encoder.decoder.treat <- data.frame(
  "p.values.treat" = basque.p.values.treated,
  "year" =  1969:1997
)

basque.encoder.decoder.control.long <- melt(basque.encoder.decoder.control, id="year")  # convert to long format
basque.encoder.decoder.control.long$group <- "Control"

basque.encoder.decoder.treat.long <- melt(basque.encoder.decoder.treat, id="year")  # convert to long format
basque.encoder.decoder.treat.long$group <- "Treated"

basque.encoder.decoder.long <- rbind(basque.encoder.decoder.treat.long, basque.encoder.decoder.control.long)

encoder.decoder.plot.pvalues.basque <- ggplot(data=basque.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("Encoder-decoder p-values: Basque Country Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-pvalues-basque.png"), encoder.decoder.plot.pvalues.basque, width=11, height=8.5)

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

basque.encoder.decoder.plot <- ggplot(data=basque.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=basque.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=basque.encoder.decoder.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1969, linetype=2) + 
  ggtitle(paste0("Encoder-decoder actual vs. counterfactual outcome: Basque Country Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-basque.png"), basque.encoder.decoder.plot, width=11, height=8.5)