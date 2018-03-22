#####################################
### ed              ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

basque.ed.preds <- read_csv(paste0(results.directory, "encoder-decoder/basque/weights.388-0.048.hdf5-basque-test.csv"), col_names = FALSE)

# Actual versus predicted
basque.ed <- data.frame(
  "y.pred" = c(rep(NA,length(basque.y.train$gdpcap.10)), basque.ed.preds[[1]]),
  "y.true" = c(basque.y.train$gdpcap.10, basque.y.test$gdpcap.10),
  "year" =  1955:1997
)

# Post-period MSPE and APE

# Post
basque.ed.MSPE <- filter(basque.ed, year %in% c(1969:1997)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
basque.ed.MSPE

basque.ed.APE <- filter(basque.ed, year %in% c(1969:1997)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(basque.ed.APE$APE)

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

ed.plot <- ggplot(data=basque.ed, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1969, linetype=2) + 
  ggtitle(paste0("Basque Country (placebo): Encoder-decoder (validation MSPE = 0.048)")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-basque.png"), ed.plot, width=11, height=8.5)