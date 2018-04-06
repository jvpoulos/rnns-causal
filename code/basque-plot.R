#####################################
### Compare Basque Country estimation accuracy ###
#####################################

library(ggplot2)
library(scales)
library(wesanderson)

# Absolute percentage estimation error

basque.ape.df <- data.frame("Year"=1969:1997,
                            "Encoder.decoder"= basque.ed.APE$APE,
                            "LSTM"= basque.lstm.APE$APE,
                            "BSTS"= basque.bsts.APE$APE,
                            "Synthetic control"= basque.synth.APE$APE)

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.80)
                     , legend.justification = c(1,0))

# Plot actual versus predicted with credible intervals for the holdout period
basque.ape.plot <- ggplot(data=basque.ape.df, aes(x=Year)) +
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
  ggtitle(paste0("Basque Country: Estimation accuracy")) +
  theme.blank + theme(legend.key.width=unit(3,"line")) 

ggsave(paste0(results.directory,"plots/basque-ape-plot.png"), basque.ape.plot, width=11, height=8.5)
