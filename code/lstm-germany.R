#####################################
### LSTM                          ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

germany.lstm.preds <- read_csv(paste0(results.directory, "lstm/germany/weights.992-0.004.hdf5-germany-test.csv"), col_names = FALSE)

# Actual versus predicted
germany.lstm <- data.frame(
  "y.pred" = c(rep(NA,length(germany.y.train$gdp.10)), germany.lstm.preds[[1]]),
  "y.true" = c(germany.y.train$gdp.10, germany.y.test$gdp.10),
  "year" =  1960:2003
)

# Post-period MSPE and APE

# Post
germany.lstm.MSPE <- filter(germany.lstm, year %in% c(1990:2003)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
germany.lstm.MSPE

germany.lstm.APE <- filter(germany.lstm, year %in% c(1990:2003)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(germany.lstm.APE$APE)

# Plot actual versus predicted with credible intervals for the holdout period

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.9)
                     , legend.justification = c(1,0))

lstm.plot <- ggplot(data=germany.lstm, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita GDP (PPP, 2002 USD)") + xlab("") +
  geom_vline(xintercept=1990, linetype=2) + 
  ggtitle(paste0("West Germany (placebo): LSTM (validation MSPE = 0.004)")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/lstm-plot-germany.png"), lstm.plot, width=11, height=8.5)