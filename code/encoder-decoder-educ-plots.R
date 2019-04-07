#####################################
### ed ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# setup

funds <- readRDS("/media/jason/Dropbox/github/land-reform/data/capacity-outcomes.rds")[['educ.pc']]

Y <- funds$M # NxT 

treated.indices <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
t0 <- which(colnames(Y)=="1869") # first treatment time # same for all outcomes

# import predictions

educ.ed.pred.treated <- read_csv(paste0(results.directory, "encoder-decoder/educ/encoder-decoder-educ-test.csv"), col_names = FALSE)
educ.ed.pred.control <- read_csv(paste0(results.directory, "encoder-decoder/educ/encoder-decoder-educ-train.csv"), col_names = FALSE)

educ.n.placebo <- ncol(educ.ed.pred.control )

# Post-period MSE (all controls)

observed <- t(Y)

educ.control.forecast <- as.matrix(educ.ed.pred.control)
educ.control.true <- as.matrix(observed[,!colnames(observed) %in% c(treated.indices,"TN")][(t0+1):nrow(observed),])

educ.ed.mse <- mean((educ.control.true-educ.control.forecast)**2) # post-intervention MSE
educ.ed.mse

educ.ed.preds <- rbind(matrix(NA, educ.n.pre, educ.n.placebo+1), as.matrix(cbind(educ.ed.pred.treated, educ.ed.pred.control))) # pad pre-period for plot

# Calculate real treated pooled intervention effect

educ.treat.forecast <-  as.matrix(educ.ed.pred.treated)

educ.treat.true <- as.matrix(educ.y[1][(educ.n.pre+1):nrow(educ.y),])

educ.t.stat <- rowMeans(educ.treat.true-educ.treat.forecast) # real t stat

# P-values for both treated and placebo treated

educ.p.values.treated <- PermutationTest(educ.control.forecast, educ.control.true, educ.t.stat, educ.n.placebo, np=10000)

educ.p.values.control <- sapply(1:educ.n.placebo, function(c){
  educ.t.stat.control <- rowMeans(as.matrix(educ.control.true[,c])-as.matrix(educ.control.forecast[,c]))
  PermutationTest(educ.control.forecast[,-c], educ.control.true[,-c], educ.t.stat.control, educ.n.placebo-1, np=10000)
})

ed.educ.fpr <- sum(educ.p.values.control <=0.05)/length(educ.p.values.control) #FPR
ed.educ.fpr
sum(p.adjust(educ.p.values.control, "bonferroni") <=0.05)/length(educ.p.values.control) # adjusted

# CIs for treated

educ.CI.treated <- PermutationCI(educ.control.forecast, educ.control.true, educ.t.stat, educ.n.placebo, c.range=c(-10,10), np=10000, l=1000)

# Plot pointwise impacts

# Pointwise impacts
educ.ed.control <- data.frame(
  "pointwise.control" = educ.x[(educ.n.pre+1):nrow(educ.x),]-educ.control.forecast,
  "year" =  educ.pc.x.imp$year[educ.pc.x.imp$year>=1862]
)

educ.ed.treat <- data.frame(
  "pointwise.treat" = educ.y[(educ.n.pre+1):nrow(educ.y),]-educ.treat.forecast, 
  "year" =  educ.pc.y.west$year[educ.pc.y.west$year>=1862]
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

educ.ed.control.long <- melt(educ.ed.control, id="year")  # convert to long format
educ.ed.control.long$group <- "Control"

educ.ed.treat.long <- melt(educ.ed.treat, id="year")  # convert to long format
educ.ed.treat.long$group <- "Treated"

educ.ed.long <- rbind(educ.ed.treat.long, educ.ed.control.long)

educ.ed.long$ymin <- NA
educ.ed.long$ymax <- NA

educ.ed.long$ymin[educ.ed.long$group=="Treated"] <- educ.CI.treated[,1]
educ.ed.long$ymax[educ.ed.long$group=="Treated"] <- educ.CI.treated[,2]

ed.plot.educ <- ggplot(data=educ.ed.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey", alpha=0.5, size=0) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita state government education spending (1982$)") + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1860, 1980, 20)) +
  scale_alpha_manual(values=c(0.1, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  coord_cartesian(ylim=c(-8, 8)) +
 # ggtitle("ed treatment effects: Education spending in West") +
  theme.blank + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/ed-plot-effects-educ.png"), ed.plot.educ, width=11, height=8.5)

# mean(educ.treat.true)-mean(as.matrix(educ.y[1][1:(educ.n.pre),])) # 1st diff (treated post-pre)
# 
# mean(educ.treat.forecast)-mean(as.matrix(educ.y[1][1:(educ.n.pre),])) # 2nd diff (counterfactual post-pre)

mean(educ.ed.long$value[educ.ed.long$variable=="X1"])
mean(educ.ed.long$ymin[educ.ed.long$variable=="X1"])
mean(educ.ed.long$ymax[educ.ed.long$variable=="X1"])

# Plot p-values

educ.ed.control <- data.frame(
  "p.values.control" = educ.p.values.control,
  "year" =  educ.pc.y.west$year[educ.pc.y.west$year>=1862]
)

educ.ed.treat <- data.frame(
  "p.values.treat" = educ.p.values.treated,
  "year" =  educ.pc.y.west$year[educ.pc.y.west$year>=1862]
)

educ.ed.control.long <- melt(educ.ed.control, id="year")  # convert to long format
educ.ed.control.long$group <- "Control"

educ.ed.treat.long <- melt(educ.ed.treat, id="year")  # convert to long format
educ.ed.treat.long$group <- "Treated"

educ.ed.long <- rbind(educ.ed.treat.long, educ.ed.control.long)

ed.plot.pvalues.educ <- ggplot(data=educ.ed.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
 # ggtitle("ed p-values: Education spending in West") +
  theme.blank + theme(legend.position = c(0.8,0.8)) + guides(colour=FALSE) + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/ed-plot-pvalues-educ.png"), ed.plot.pvalues.educ, width=11, height=8.5)

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

educ.ed.plot <- ggplot(data=educ.ed, aes(x=year)) +
  geom_line(aes(y=educ.y[[1]], colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=educ.ed.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita state government education spending (1982$)") + xlab("") +
  geom_vline(xintercept=1862, linetype=2) + 
 # ggtitle(paste0("ed actual vs. counterfactual outcome: Education spending in West")) +
  theme.blank + theme(legend.position="none")

ggsave(paste0(results.directory,"plots/ed-plot-educ.png"), educ.ed.plot, width=11, height=8.5)