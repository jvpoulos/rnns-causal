#####################################
### linear                        ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

for(t in 1:germany.n.post){
  if(t==1){
    # Train
    linear.reg <- glm(x = as.ts(cbind(germany.y[1:germany.n.pre,], germany.x[1:germany.n.pre,])), 
                     demean = TRUE, intercept = TRUE) # linear model is x minus its mean (imp for W. Germany)
    # Predict
    p.test <- predict(linear.reg, 
                      newdata = as.ts(cbind(germany.y[1:germany.n.pre,], germany.x[1:germany.n.pre,])), 
                      n.ahead =  1, 
                      se.fit=FALSE)
    
    germany.linear.pred <- c(p.test)
    
  }else{
    end <- (germany.n.pre-1)+t
    # Train
    linear.reg <- update(linear.reg, x = as.ts(cbind(germany.y[t:end,], germany.x[t:end,]))) 
    
    # Predict
    p.test <- predict(linear.reg, 
                      newdata = as.ts(cbind(germany.y[t:end,], germany.x[t:end,])), 
                      n.ahead =  1,
                      se.fit=FALSE)
    
    germany.linear.pred <- rbind(germany.linear.pred, p.test)
  }
}

# Actual versus predicted
germany.linear <- data.frame(
  "y.pred" = rbind(matrix(NA, germany.n.pre, germany.n.placebo+1), as.matrix(germany.linear.pred)),
  "y.true" = cbind(germany.y, germany.x),
  "year" =  1960:2003
)

# Post-period MSE and MAPE (all controls)

germany.control.forecast <- as.matrix(germany.linear.pred[,-1])
germany.control.true <- as.matrix(germany.x[(germany.n.pre+1):nrow(germany.x),])

germany.linear.mse <- error(forecast=germany.control.forecast, true=germany.control.true, method = "mse") # post-intervention MSE

germany.linear.preds <- rbind(matrix(NA, germany.n.pre, germany.n.placebo+1), as.matrix(germany.linear.pred)) # pad pre-period for plot

# Calculate real treated pooled intervention effect

germany.treat.forecast <-  as.matrix(germany.linear.pred[,1])

germany.treat.true <- as.matrix(germany.y[1][(germany.n.pre+1):nrow(germany.y),])

germany.t.stat <- rowMeans(germany.treat.true-germany.treat.forecast) # real t stat

# P-values for both treated and placebo treated

germany.p.values.treated <- PermutationTest(germany.control.forecast, germany.control.true, germany.t.stat, germany.n.placebo)

germany.p.values.control <- sapply(1:length(germany.controls), function(c){
  germany.t.stat.control <- rowMeans(as.matrix(germany.control.true[,c])-as.matrix(germany.control.forecast[,c]))
  PermutationTest(germany.control.forecast[,-c], germany.control.true[,-c], germany.t.stat.control, germany.n.placebo-1)
})

linear.germany.fpr <-sum(germany.p.values.control <=0.05)/length(germany.p.values.control) #FPR

# Plot pointwise impacts

# Pointwise impacts
germany.linear.control <- data.frame(
  "pointwise.control" = germany.x[(germany.n.pre+1):nrow(germany.x),]-germany.control.forecast,
  "year" =  1990:2003
)

germany.linear.treat <- data.frame(
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
                     , legend.position = c(0.2,0.2)
                     , legend.justification = c(1,0))

germany.linear.control.long <- melt(germany.linear.control, id="year")  # convert to long format
germany.linear.control.long$group <- "Control"

germany.linear.treat.long <- melt(germany.linear.treat, id="year")  # convert to long format
germany.linear.treat.long$group <- "Treated"

germany.linear.long <- rbind(germany.linear.treat.long, germany.linear.control.long)

linear.plot.germany <- ggplot(data=germany.linear.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log log per-capita GDP (PPP, 2002 USD)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("Linear Model Treatment Effects: West Germany Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/linear-plot-effects-germany.png"), linear.plot.germany, width=11, height=8.5)

# Plot p-values

germany.linear.control <- data.frame(
  "p.values.control" = germany.p.values.control,
  "year" =  1990:2003
)

germany.linear.treat <- data.frame(
  "p.values.treat" = germany.p.values.treated,
  "year" =  1990:2003
)

germany.linear.control.long <- melt(germany.linear.control, id="year")  # convert to long format
germany.linear.control.long$group <- "Control"

germany.linear.treat.long <- melt(germany.linear.treat, id="year")  # convert to long format
germany.linear.treat.long$group <- "Treated"

germany.linear.long <- rbind(germany.linear.treat.long, germany.linear.control.long)

linear.plot.pvalues.germany <- ggplot(data=germany.linear.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("Linear model p-values: West Germany Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/linear-plot-pvalues-germany-no-predictors.png"), linear.plot.pvalues.germany, width=11, height=8.5)

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

germany.linear.plot <- ggplot(data=germany.linear, aes(x=year)) +
  geom_line(aes(y=germany.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=germany.linear.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita GDP (PPP, 2002 USD))") + xlab("") +
  geom_vline(xintercept=1990, linetype=2) + 
  ggtitle(paste0("Linear model actual vs. counterfactual outcome: West Germany Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/linear-plot-germany.png"), germany.linear.plot, width=11, height=8.5)