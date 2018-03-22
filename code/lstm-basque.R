#####################################
### LSTM              ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

basque.lstm.preds <- read_csv(paste0(results.directory, "lstm/basque/weights.987-0.012.hdf5-basque-test.csv"), col_names = FALSE)

# Actual versus predicted
basque.lstm <- data.frame(
  "y.pred" = c(rep(NA,length(basque.y.train$gdpcap.10)), basque.lstm.preds[[1]]),
  "y.true" = c(basque.y.train$gdpcap.10, basque.y.test$gdpcap.10),
  "year" =  1955:1997
)

# Post-period MSPE and APE

# Post
basque.lstm.MSPE <- filter(basque.lstm, year %in% c(1969:1997)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
basque.lstm.MSPE

basque.lstm.APE <- filter(basque.lstm, year %in% c(1969:1997)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(basque.lstm.APE$APE)

# Plot actual versus predicted with credible intervals for the holdout period

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.9,0.9)
                     , legend.justification = c(1,0))

lstm.plot <- ggplot(data=basque.lstm, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1969, linetype=2) + 
  ggtitle(paste0("Basque Country (placebo): LSTM (validation MSPE = 0.012)")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/lstm-plot-basque.png"), lstm.plot, width=11, height=8.5)