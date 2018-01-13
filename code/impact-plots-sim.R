# Plot time-series and estimate causal impacts

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)
library(wesanderson)

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"
data.directory <-"~/Dropbox/github/rnns-causal/data/"

n.pre <- 47
n.post <- 5
output.dim <- 5

autoencoder <- TRUE

# Get splits

y.train <- read.csv(paste0(data.directory,"elections/sim/sim_y_train_treated.csv"), header=FALSE)

y.test <- read.csv(paste0(data.directory,"elections/sim/sim_y_test_treated.csv"), header=FALSE)

phi <- -abs(y.test*0.1)

y.test.c <- y.test + phi  #true counterfactual

# Import test results 

if(autoencoder){
  setwd(paste0(results.directory, "elections-auto/sim")) # prediction files loc
} else{
  setwd(paste0(results.directory, "elections/sim")) # prediction files loc
}

test.files <- list.files(pattern = "*test.csv")

votediff.preds.test <- lapply(test.files,function(x){
  m <- read.csv(x, header=FALSE)
  return(as.matrix(m))})

votediff.preds.test.sd <- apply(simplify2array(votediff.preds.test), 1:2, sd)

#votediff.preds.test.mean <- apply(simplify2array(votediff.preds.test), 1:2, mean) # element-wise mean

if(autoencoder){
  best.model <- 9
} else{
  best.model <-6
}
votediff.preds.test.mean <- votediff.preds.test[[best.model]] # best model

# Bind predictions

votediff.bind.sim <- data.frame("y.true"=rbind(y.train,y.test), 
                                "y.true.c"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), y.test.c),
                                "phi"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), phi),
                                "y.pred"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), votediff.preds.test.mean),
                                "y.sd"=rbind(matrix(NA, nrow=n.pre, ncol=output.dim), votediff.preds.test.sd))


## Create time series data
setwd(code.directory)

## Plot time series 

# Combine /take means across features

votediff.bind.sim <- data.frame("year"=1:nrow(votediff.bind.sim),
                                      "y.pred"=rowMeans(votediff.bind.sim[16:20], na.rm = TRUE),
                                      "y.true"=rowMeans(votediff.bind.sim[1:5], na.rm = TRUE),
                                      "y.true.c"=rowMeans(votediff.bind.sim[6:10], na.rm = TRUE),
                                      "y.phi"=rowMeans(votediff.bind.sim[11:15], na.rm = TRUE),
                                      "y.sd"=rowMeans(votediff.bind.sim[21:25], na.rm = TRUE))

votediff.bind.sim <- votediff.bind.sim  %>%
  mutate(pred.votediff.min = y.pred - y.sd*1.96,
         pred.votediff.max = y.pred + y.sd*1.96,
         pointwise.votediff = y.true - y.pred,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)


theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.3,0.8)
                     , legend.justification = c(1,0))
if(autoencoder){
  sim.plot.title <- "Simulated data: Autoencoder (training MSPE = 0.003; validation MSPE = 0.25)"
} else{
  "Simulated data: Encoder-decoder (training MSPE = 0.52; validation MSPE = 0.32)"
}

# Plot actual versus predicted with credible intervals for the holdout period
ed.sim.plot <- ggplot(data=votediff.bind.sim, aes(x=1:52)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome", linetype= "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome", linetype = "Predicted treated outcome"), size=1.2) +
  geom_line(aes(y=y.true.c, colour = "True counterfactual", linetype = "True counterfactual"), size=1.2) +
  scale_linetype_manual("",values=c("Observed treated outcome"="solid","Predicted treated outcome"="dashed","True counterfactual"="dotted"),
                        labels=c("Observed treated outcome", "Predicted treated outcome","True counterfactual")) +
  scale_colour_manual(name="", values = c("Observed treated outcome" = wes_palette("Darjeeling2")[3], "Predicted treated outcome" = wes_palette("Darjeeling2")[2], "True counterfactual" = wes_palette("Darjeeling2")[5]),
                      labels=c("Observed treated outcome", "Predicted treated outcome","True counterfactual")) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("ARMA time-series") + xlab("Time-step") +
  geom_vline(xintercept=48, linetype=2) + 
  geom_ribbon(aes(ymin= pred.votediff.min, ymax=pred.votediff.max), fill="grey", alpha=0.5) +
  ggtitle(sim.plot.title) +
  theme.blank + theme(legend.key.width=unit(3,"line"))

if(autoencoder){
  ggsave(paste0(results.directory,"plots/impact-sim-auto.png"), ed.sim.plot, width=11, height=8.5)
} else{
  ggsave(paste0(results.directory,"plots/impact-sim.png"), ed.sim.plot, width=11, height=8.5)
}

# Absolute percentage estimation error

if(autoencoder){
auto.sim.APE <- filter(votediff.bind.sim, !is.na(pointwise.votediff)) %>% mutate(APE=abs(pointwise.votediff-y.phi)/abs(y.phi))
} else{
  ed.sim.APE <- filter(votediff.bind.sim, !is.na(pointwise.votediff)) %>% mutate(APE=abs(pointwise.votediff-y.phi)/abs(y.phi))
}