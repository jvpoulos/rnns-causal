#####################################
### Compare germany estimation accuracy ###
#####################################

library(ggplot2)
library(scales)
library(wesanderson)

# Absolute percentage estimation error

germany.ape.df <- data.frame("Year"=1990:2003,
                            "Encoder.decoder"= germany.encoder.decoder.APE$APE,
                            "LSTM"= germany.lstm.APE$APE,
                            "BSTS"= germany.bsts.APE$APE,
                            "Synthetic control"= germany.synth.APE$APE)

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.8)
                     , legend.justification = c(1,0))

# Plot actual versus predicted with credible intervals for the holdout period
germany.ape.plot <- ggplot(data=germany.ape.df, aes(x=Year)) +
  geom_line(aes(y=BSTS, colour = "BSTS", linetype = "BSTS"), size=1.2) +
  geom_line(aes(y=Encoder.decoder, colour = "Encoder-decoder", linetype = "Encoder-decoder"), size=1.2) +
  geom_line(aes(y=LSTM, colour = "LSTM", linetype = "LSTM"), size=1.2) +
  geom_line(aes(y=Synthetic.control, colour = "Synthetic control", linetype = "Synthetic control"), size=1.2) +
  scale_linetype_manual("Model",values=c("BSTS"=3,"Encoder-decoder"=4,"LSTM"=5,"Synthetic control"=6),
                        labels=c("BSTS", "Encoder-decoder","LSTM","Synthetic control")) +
  scale_colour_manual(name="Model", values = c("BSTS" = wes_palette("Darjeeling")[4], "Encoder-decoder" = wes_palette("Darjeeling")[2], "LSTM" = wes_palette("Darjeeling")[3],"Synthetic control" = wes_palette("Darjeeling")[5]),
                      labels=c("BSTS", "Encoder-decoder", "LSTM","Synthetic control")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Absolute prediction error (APE)") + xlab("") +
  ggtitle(paste0("West Germany: Estimation accuracy")) +
  theme.blank + theme(legend.key.width=unit(3,"line")) 

ggsave(paste0(results.directory,"plots/germany-ape-plot.png"), germany.ape.plot, width=11, height=8.5)
