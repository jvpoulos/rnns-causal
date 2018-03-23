#####################################
### LSTM                      ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

votediff.lstm.preds <- read_csv(paste0(results.directory, "lstm/votediff/weights.4795-4.678.hdf5-votediff-test.csv"), col_names = FALSE)
votediff.lstm.preds <- rowMeans(votediff.lstm.preds)

# Actual versus predicted
votediff.lstm <- data.frame(
  "y.pred" = c(rep(NA,nrow(votediff.y.train)), votediff.lstm.preds),
  "y.true" = rowMeans(votediff.y[colnames(votediff.y) %in% colnames(votediff.y.test)][-1], na.rm=TRUE), # nonimputed
  "year" =  votediff.y$year
)

votediff.lstm$pointwise <- votediff.lstm$y.true-votediff.lstm$y.pred

# Post-period MSPE

# Post
votediff.lstm.MSPE <- filter(votediff.lstm, year %in% c(2005:2010)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
votediff.lstm.MSPE

votediff.lstm.APE <- filter(votediff.lstm, year %in% c(2005:2010)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(votediff.lstm.APE$APE)

# Plot actual versus predicted with credible intervals for the holdout period

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.35,0.25)
                     , legend.justification = c(1,0))

elections.lstm.plot <- ggplot(data=votediff.lstm, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Winner margin (ln)") + xlab("") +
  geom_vline(xintercept=2005, linetype=2) + 
  ggtitle("Mayoral elections: LSTM (validation MSPE = 4.678)") +
  theme.blank 

ggsave(paste0(results.directory,"plots/lstm-votediff.png"), elections.lstm.plot, width=11, height=8.5)