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

california.ed.preds <- read_csv(paste0(results.directory, "encoder-decoder/california/weights.390-0.013.hdf5-california-test.csv"), col_names = FALSE)

# Actual versus predicted
california.ed <- data.frame(
  "y.pred" = c(rep(NA,length(california.y.train$cigsale.23)), california.ed.preds[[1]]),
  "y.true" = c(california.y.train$cigsale.23, california.y.test$cigsale.23),
  "year" =  1970:2000
)

# Post-period MSPE and APE

# Post
california.ed.MSPE <- filter(california.ed, year %in% c(1989:2000)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
california.ed.MSPE

california.ed.APE <- filter(california.ed, year %in% c(1989:2000)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(california.ed.APE$APE)

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

ed.plot <- ggplot(data=california.ed, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Log per-capita cigarette sales (in packs)") + xlab("") +
  geom_vline(xintercept=1989, linetype=2) + 
  ggtitle(paste0("California (placebo): Encoder-decoder (validation MSPE = 0.013)")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/encoder-decoder-plot-california.png"), ed.plot, width=11, height=8.5)