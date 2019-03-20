#####################################
### encoder.decoder ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

votediff.n.pre <- 47
votediff.n.placebo <- 778
votediff.n.treated <- 24

# import predictions

votediff.encoder.decoder.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/votediff/treated/weights.4830-0.357.hdf5-votediff-test.csv"), col_names = FALSE)
votediff.encoder.decoder.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/votediff/control/weights.3670-0.055.hdf5-votediff-test.csv"), col_names = FALSE)

# Actual versus predicted
votediff.encoder.decoder <- data.frame(
  "y.pred" = rbind(matrix(NA, votediff.n.pre, votediff.n.placebo+votediff.n.treated), as.matrix(cbind(votediff.encoder.decoder.pred.treated, votediff.encoder.decoder.pred.control))),
  "y.true" = cbind(votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")], votediff.x[!colnames(votediff.x) %in% c("year")]), # imputed labels
  "year" =  votediff.y.imp$year
)

# Post-period MSE and MAPE (all controls)

votediff.control.forecast <- as.matrix(votediff.encoder.decoder.pred.control)
votediff.control.true <- as.matrix(votediff.x[!colnames(votediff.x) %in% c("year")][(votediff.n.pre+1):nrow(votediff.x),])

votediff.encoder.decoder.mse <- mean((votediff.control.true-votediff.control.forecast)**2) # post-intervention MSE
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
(0.0005 - mean(votediff.t.stat[1:2]))**2 # MSPE pooled

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

# Plot pointwise impacts

# Pointwise impacts
votediff.encoder.decoder.control <- data.frame(
  "pointwise.control" = votediff.x[!colnames(votediff.x) %in% c("year")][(votediff.n.pre+1):nrow(votediff.x),]-votediff.control.forecast,
  "year" =  c(2005:2007,2009,2010)
)

votediff.encoder.decoder.treat <- data.frame(
  "pointwise.treat" = votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")][(votediff.n.pre+1):nrow(votediff.y.imp),]-votediff.treat.forecast, 
  "year" =  c(2005:2007,2009,2010)
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

votediff.encoder.decoder.control.long <- melt(votediff.encoder.decoder.control, id="year")  # convert to long format
votediff.encoder.decoder.control.long$group <- "Control"

votediff.encoder.decoder.treat.long <- melt(votediff.encoder.decoder.treat, id="year")  # convert to long format
votediff.encoder.decoder.treat.long$group <- "Treated"

votediff.encoder.decoder.long <- rbind(votediff.encoder.decoder.treat.long, votediff.encoder.decoder.control.long)

votediff.encoder.decoder.long$ymin <- NA
votediff.encoder.decoder.long$ymax <- NA

votediff.encoder.decoder.long$ymin[votediff.encoder.decoder.long$group=="Treated"] <- votediff.CI.treated[,1]
votediff.encoder.decoder.long$ymax[votediff.encoder.decoder.long$group=="Treated"] <- votediff.CI.treated[,2]

encoder.decoder.plot.votediff <- ggplot(data=votediff.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log winner margin") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
#  ggtitle("Encoder-decoder Treatment Effects: Elections Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-effects-votediff.png"), encoder.decoder.plot.votediff, width=11, height=8.5)

# Plot p-values

votediff.encoder.decoder.control <- data.frame(
  "p.values.control" = votediff.p.values.control,
  "year" =  1945:2010
)

votediff.encoder.decoder.treat <- data.frame(
  "p.values.treat" = votediff.p.values.treated,
  "year" =  1945:2010
)

votediff.encoder.decoder.control.long <- melt(votediff.encoder.decoder.control, id="year")  # convert to long format
votediff.encoder.decoder.control.long$group <- "Control"

votediff.encoder.decoder.treat.long <- melt(votediff.encoder.decoder.treat, id="year")  # convert to long format
votediff.encoder.decoder.treat.long$group <- "Treated"

votediff.encoder.decoder.long <- rbind(votediff.encoder.decoder.treat.long, votediff.encoder.decoder.control.long)

encoder.decoder.plot.pvalues.votediff <- ggplot(data=votediff.encoder.decoder.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
#  ggtitle("Encoder-decoder p-values: Elections Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-pvalues-votediff.png"), encoder.decoder.plot.pvalues.votediff, width=11, height=8.5)

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

votediff.encoder.decoder.plot <- ggplot(data=votediff.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=votediff.y.imp[!colnames(votediff.y.imp) %in% c("year")], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=votediff.encoder.decoder.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log winner margin") + xlab("") +
  geom_vline(xintercept=2005, linetype=2) + 
#  ggtitle(paste0("Encoder-decoder actual vs. counterfactual outcome: Elections Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-votediff.png"), votediff.encoder.decoder.plot, width=11, height=8.5)