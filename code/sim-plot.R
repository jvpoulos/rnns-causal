#####################################
### Compare Simulation data estimation accuracy ###
#####################################

library(ggplot2)
library(wesanderson)

# Absolute percentage estimation error

sim.ape.df <- data.frame("t"=48:52,
                            "Encoder.decoder"= sim.encoder.decoder.APE$APE,
                            "LSTM"= sim.lstm.APE$APE,
                            "BSTS"= bsts.sim.APE$APE,
                            "Synthetic control"= synth.sim.APE$APE)

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.85,0.80)
                     , legend.justification = c(1,0))

# Plot actual versus predicted with credible intervals for the holdout period
sim.ape.plot <- ggplot(data=sim.ape.df, aes(x=t)) +
  geom_line(aes(y=BSTS, colour = "BSTS", linetype = "BSTS"), size=1.2) +
  geom_line(aes(y=Encoder.decoder, colour = "Encoder-decoder", linetype = "Encoder-decoder"), size=1.2) +
  geom_line(aes(y=LSTM, colour = "LSTM", linetype = "LSTM"), size=1.2) +
  geom_line(aes(y=Synthetic.control, colour = "Synthetic control", linetype = "Synthetic control"), size=1.2) +
  scale_linetype_manual("Model",values=c("BSTS"=3,"Encoder-decoder"=4,"LSTM"=5,"Synthetic control"=6),
                        labels=c("BSTS", "Encoder-decoder","LSTM","Synthetic control")) +
  scale_colour_manual(name="Model", values = c("BSTS" = wes_palette("Darjeeling")[4], "Encoder-decoder" = wes_palette("Darjeeling")[2], "LSTM" = wes_palette("Darjeeling")[3],"Synthetic control" = wes_palette("Darjeeling")[5]),
                      labels=c("BSTS", "Encoder-decoder", "LSTM","Synthetic control")) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Absolute prediction error (APE)") + xlab("t'") +
  ggtitle(paste0("Simulation data: Estimation accuracy")) +
  theme.blank + theme(legend.key.width=unit(3,"line")) 

ggsave(paste0(results.directory,"plots/sim-ape-plot.png"), sim.ape.plot, width=11, height=8.5)
