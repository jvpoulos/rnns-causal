#####################################
### linear ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

for(t in 1:california.n.post){
  if(t==1){
    # Train
    linear.reg <- glm(x = as.ts(cbind(california.y[1:california.n.pre,], california.x[1:california.n.pre,])), 
                     demean = TRUE, intercept = TRUE) 
    # Predict
    p.test <- predict(linear.reg, 
                      newdata = as.ts(cbind(california.y[1:california.n.pre,], california.x[1:california.n.pre,])), 
                      n.ahead =  1, 
                      se.fit=FALSE)
    
    california.linear.pred <- c(p.test)
    
  }else{
    end <- (california.n.pre-1)+t
    # Train
    linear.reg <- update(linear.reg, x = as.ts(cbind(california.y[t:end,], california.x[t:end,]))) 
    
    # Predict
    p.test <- predict(linear.reg, 
                      newdata = as.ts(cbind(california.y[t:end,], california.x[t:end,])), 
                      n.ahead =  1,
                      se.fit=FALSE)
    
    california.linear.pred <- rbind(california.linear.pred, p.test)
  }
}

# Actual versus predicted
california.linear <- data.frame(
  "y.pred" = rbind(matrix(NA, california.n.pre, california.n.placebo+1), as.matrix(california.linear.pred)),
  "y.true" = cbind(california.y, california.x),
  "year" =  1970:2000
)

# Post-period MSE and MAPE (all controls)

california.control.forecast <- as.matrix(california.linear.pred[,-1])
california.control.true <- as.matrix(california.x[(california.n.pre+1):nrow(california.x),])

california.linear.mse <- error(forecast=california.control.forecast, true=california.control.true, method = "mse") # post-intervention MSE

california.linear.preds <- rbind(matrix(NA, california.n.pre, california.n.placebo+1), as.matrix(california.linear.pred)) # pad pre-period for plot

# Calculate real treated pooled intervention effect

california.treat.forecast <-  as.matrix(california.linear.pred[,1])

california.treat.true <- as.matrix(california.y[1][(california.n.pre+1):nrow(california.y),])

california.t.stat <- rowMeans(california.treat.true-california.treat.forecast) # real t stat

# P-values for both treated and placebo treated

california.p.values.treated <- PermutationTest(california.control.forecast, california.control.true, california.t.stat, california.n.placebo, np=10000)

california.p.values.control <- sapply(1:length(california.controls), function(c){
  california.t.stat.control <- rowMeans(as.matrix(california.control.true[,c])-as.matrix(california.control.forecast[,c]))
  PermutationTest(california.control.forecast[,-c], california.control.true[,-c], california.t.stat.control, california.n.placebo-1, np=10000)
})

linear.california.fpr <- sum(california.p.values.control <=0.05)/length(california.p.values.control) #FPR

# Plot pointwise impacts

# Pointwise impacts
california.linear.control <- data.frame(
  "pointwise.control" = california.x[(california.n.pre+1):nrow(california.x),]-california.control.forecast,
  "year" =  1989:2000
)

california.linear.treat <- data.frame(
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
                     , legend.position = c(0.2,0.15)
                     , legend.justification = c(1,0))

california.linear.control.long <- melt(california.linear.control, id="year")  # convert to long format
california.linear.control.long$group <- "Control"

california.linear.treat.long <- melt(california.linear.treat, id="year")  # convert to long format
california.linear.treat.long$group <- "Treated"

california.linear.long <- rbind(california.linear.treat.long, california.linear.control.long)

linear.plot.california <- ggplot(data=california.linear.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_line() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Treatment effects on log per-capita cigarette sales (in packs)") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=2) + 
  ggtitle("Linear Model Treatment Effects: California Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/linear-plot-effects-california.png"), linear.plot.california, width=11, height=8.5)

# Plot p-values

california.linear.control<- data.frame(
  "p.value" = california.p.values.control, 
  "year" =  1989:2000
)

california.linear.treat<- data.frame(
  "p.value" = california.p.values.treated, 
  "year" =  1989:2000
)

california.linear.control.long <- melt(california.linear.control, id="year")  # convert to long format
california.linear.control.long$group <- "Control"

california.linear.treat.long <- melt(california.linear.treat, id="year")  # convert to long format
california.linear.treat.long$group <- "Treated"

california.linear.long <- rbind(california.linear.treat.long, california.linear.control.long)

linear.plot.pvalues.california <- ggplot(data=california.linear.long, aes(x=year, y=value, colour=variable, size=group, alpha=group)) +
  geom_point() +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("p-value") + 
  xlab("Year") +
  scale_alpha_manual(values=c(0.2, 0.9)) +
  scale_size_manual(values=c(0.8, 2)) +
  geom_hline(yintercept=0, linetype=1) + 
  geom_hline(yintercept=0.05, linetype=2, colour="red") + 
  ggtitle("Linear model p-values: California Dataset") +
  theme.blank + guides(colour=FALSE)

ggsave(paste0(results.directory,"plots/linear-plot-pvalues-california.png"), linear.plot.pvalues.california, width=11, height=8.5)

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

california.linear.plot <- ggplot(data=california.linear, aes(x=year)) +
  geom_line(aes(y=california.y, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=california.linear.preds[,1], colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita cigarette sales (in packs))") + xlab("") +
  geom_vline(xintercept=1989, linetype=2) + 
  ggtitle(paste0("Linear model actual vs. counterfactual outcome: California Dataset")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/linear-plot-california.png"), california.linear.plot, width=11, height=8.5)