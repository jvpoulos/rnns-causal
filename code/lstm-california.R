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

california.lstm.preds <- read_csv(paste0(results.directory, "lstm/california/weights.978-0.004.hdf5-california-test.csv"), col_names = FALSE)

# Actual versus predicted
california.lstm <- data.frame(
  "y.pred" = c(rep(NA,length(california.y.train$cigsale.23)), california.lstm.preds[[1]]),
  "y.true" = c(california.y.train$cigsale.23, california.y.test$cigsale.23),
  "year" =  1970:2000
)

# Post-period MSPE and APE

# Post
california.lstm.MSPE <- filter(california.lstm, year %in% c(1989:2000)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
california.lstm.MSPE

california.lstm.APE <- filter(california.lstm, year %in% c(1989:2000)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(california.lstm.APE$APE)

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

lstm.plot <- ggplot(data=california.lstm, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita cigarette sales (in packs)") + xlab("") +
  geom_vline(xintercept=1989, linetype=2) + 
  ggtitle(paste0("California (placebo): LSTM (validation MSPE = 0.004)")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/lstm-plot-california.png"), lstm.plot, width=11, height=8.5)