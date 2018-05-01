#####################################
### Linear                        ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

for(t in 1:basque.n.post){
  if(t==1){
    # Train
    linear.reg <- glm(x = as.ts(cbind(basque.y[1:basque.n.pre,], basque.x[1:basque.n.pre,])), 
                     demean = TRUE, intercept = TRUE) 
    # Predict
    p.test <- predict(linear.reg, 
                      newdata = as.ts(cbind(basque.y[1:basque.n.pre,], basque.x[1:basque.n.pre,])), 
                      n.ahead =  1, 
                      se.fit=FALSE)
    
    basque.linear.pred <- c(p.test)
    
  }else{
    end <- (basque.n.pre-1)+t
    # Train
    linear.reg <- update(linear.reg, x = as.ts(cbind(basque.y[t:end,], basque.x[t:end,]))) 
    
    # Predict
    p.test <- predict(linear.reg, 
                      newdata = as.ts(cbind(basque.y[t:end,], basque.x[t:end,])), 
                      n.ahead =  1,
                      se.fit=FALSE)
    
    basque.linear.pred <- rbind(basque.linear.pred, p.test)
  }
}

# Actual versus predicted
basque.linear <- data.frame(
  "y.pred" = rbind(matrix(NA, basque.n.pre, basque.n.placebo+1), as.matrix(basque.linear.pred)),
  "y.true" = cbind(basque.y, basque.x),
  "year" =  1955:1997
)

# Post-period MSE and MAPE (all controls)

basque.control.forecast <- as.matrix(basque.linear.pred[,-1])
basque.control.true <- as.matrix(basque.x[(basque.n.pre+1):nrow(basque.x),])

basque.linear.mse <- error(forecast=basque.control.forecast, true=basque.control.true, method = "mse") # post-intervention MSE

basque.linear.preds <- rbind(matrix(NA, basque.n.pre, basque.n.placebo+1), as.matrix(basque.linear.pred)) # pad pre-period for plot

# Calculate real treated pooled intervention effect

basque.treat.forecast <-  as.matrix(basque.linear.pred[,1])

basque.treat.true <- as.matrix(basque.y[1][(basque.n.pre+1):nrow(basque.y),])

basque.t.stat <- rowMeans(basque.treat.true-basque.treat.forecast) # real t stat

# P-values for both treated and placebo treated

basque.p.values.treated <- PermutationTest(basque.control.forecast, basque.control.true, basque.t.stat, basque.n.placebo)

basque.p.values.control <- sapply(1:length(basque.controls), function(c){
  basque.t.stat.control <- rowMeans(as.matrix(basque.control.true[,c])-as.matrix(basque.control.forecast[,c]))
  PermutationTest(basque.control.forecast[,-c], basque.control.true[,-c], basque.t.stat.control, basque.n.placebo-1)
})

linear.basque.fpr <- sum(basque.p.values.control <=0.05)/length(basque.p.values.control) #FPR

# Plot pointwise impacts

# Pointwise impacts
basque.linear.control <- data.frame(
  "pointwise.control" = basque.x[(basque.n.pre+1):nrow(basque.x),]-basque.control.forecast,
  "year" =  1969:1997
)

basque.linear.treat <- data.frame(
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
                     , legend.position = c(0.2,0.2)
                     , legend.justification = c(1,0))

basque.linear.control.long <- melt(basque.linear.control, id="year")  # convert to long format
basque.linear.control.long$group <- "Control"

basque.linear.treat.long <- melt(basque.linear.treat, id="year")  # convert to long format
basque.linear.treat.long$group <- "Treated"

basque.linear.long <- rbind(basque.linear.treat.long, basque.linear.control.long)

linear.plot.basque <- ggplot(data=basque.linear.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log real per-capita GDP (1986 USD, thousand)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("Linear Model Treatment Effects: Basque Country Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/linear-plot-effects-basque.png"), linear.plot.basque, width=11, height=8.5)

# Plot p-values

basque.linear.control <- data.frame(
  "p.values.control" = basque.p.values.control,
  "year" =  1969:1997
)

basque.linear.treat <- data.frame(
  "p.values.treat" = basque.p.values.treated,
  "year" =  1969:1997
)

basque.linear.control.long <- melt(basque.linear.control, id="year")  # convert to long format
basque.linear.control.long$group <- "Control"

basque.linear.treat.long <- melt(basque.linear.treat, id="year")  # convert to long format
basque.linear.treat.long$group <- "Treated"

basque.linear.long <- rbind(basque.linear.treat.long, basque.linear.control.long)

linear.plot.pvalues.basque <- ggplot(data=basque.linear.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("Linear model p-values: Basque Country Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/linear-plot-pvalues-basque-no-predictors.png"), linear.plot.pvalues.basque, width=11, height=8.5)

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

basque.linear.plot <- ggplot(data=basque.linear, aes(x=year)) +
  geom_line(aes(y=basque.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=basque.linear.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1969, linetype=2) + 
  ggtitle(paste0("Linear model actual vs. counterfactual outcome: Basque Country Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/linear-plot-basque.png"), basque.linear.plot, width=11, height=8.5)