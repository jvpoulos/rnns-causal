#####################################
### encoder-decoder                          ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(wesanderson)

n.pre <- 47
n.post <- 5
output.dim <- 5

autoencoder <- FALSE

# Get splits

y.train <- read.csv(paste0(data.directory,"sim/treated/sim_y_train_treated.csv"), header=FALSE)
y.train <- rowMeans(y.train)

y.test <- read.csv(paste0(data.directory,"sim/treated/sim_y_test_treated.csv"), header=FALSE)
y.test <- rowMeans(y.test)

phi <- -abs(y.test*0.1)

y.test.c <- y.test + phi  #true counterfactual

# import predictions

sim.encoder.decoder.preds <- read_csv(paste0(results.directory, "encoder-decoder/sim/weights.318-0.599.hdf5-sim-test.csv"), col_names = FALSE)
sim.encoder.decoder.preds <- rowMeans(sim.encoder.decoder.preds)

# Actual versus predicted
sim.encoder.decoder <- data.frame(
  "y.pred" = c(rep(NA,length(y.train)), sim.encoder.decoder.preds),
  "y.true" = c(y.train, y.test),
  "y.phi" = c(rep(NA,length(y.train)), phi),
  "y.true.c" =  c(rep(NA,length(y.train)), y.test.c),
  "x" =  1:(n.post+n.pre)
)

sim.encoder.decoder$pointwise <- sim.encoder.decoder$y.true-sim.encoder.decoder$y.pred

# Post-period MSPE using true counterfactual

# Post
sim.encoder.decoder.MSPE <- filter(sim.encoder.decoder, x %in% c(48:52)) %>% summarise(MSPE=mean((y.true.c-y.pred)**2))
sim.encoder.decoder.MSPE

sim.encoder.decoder.APE <- filter(sim.encoder.decoder, x %in% c(48:52)) %>% mutate(APE=abs(y.pred-y.true.c)/abs(y.true.c))
mean(sim.encoder.decoder.APE$APE)

# Plot actual versus predicted with credible intervals for the holdout period

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.3,0.8)
                     , legend.justification = c(1,0))

ed.sim.plot <- ggplot(data=sim.encoder.decoder, aes(x=x)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome", linetype= "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome", linetype = "Predicted treated outcome"), size=1.2) +
  geom_line(aes(y=y.true.c, colour = "True counterfactual", linetype = "True counterfactual"), size=1.2) +
  scale_linetype_manual("",values=c("Observed treated outcome"="solid","Predicted treated outcome"="dashed","True counterfactual"="dotted"),
                        labels=c("Observed treated outcome", "Predicted treated outcome","True counterfactual")) +
  scale_colour_manual(name="", values = c("Observed treated outcome" = wes_palette("Darjeeling2")[3], "Predicted treated outcome" = wes_palette("Darjeeling2")[2], "True counterfactual" = wes_palette("Darjeeling2")[5]),
                      labels=c("Observed treated outcome", "Predicted treated outcome","True counterfactual")) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("ARMA time-series") + xlab("Time-step") +
  geom_vline(xintercept=48, linetype=2) + 
  ggtitle("Simulated data: Encoder-decoder (validation MSPE = 0.599)") +
  theme.blank + theme(legend.key.width=unit(3,"line")) 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-sim.png"), ed.sim.plot, width=11, height=8.5)