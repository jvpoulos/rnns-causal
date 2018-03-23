#####################################
### encoder-decoder                          ### 
#####################################

library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

# import predictions

votediff.encoder.decoder.preds <- read_csv(paste0(results.directory, "encoder-decoder/votediff/weights.486-0.521.hdf5-votediff-test.csv"), col_names = FALSE)
votediff.encoder.decoder.preds <- rowMeans(votediff.encoder.decoder.preds)

# Actual versus predicted
votediff.encoder.decoder <- data.frame(
  "y.pred" = c(rep(NA,nrow(votediff.y.train)), votediff.encoder.decoder.preds),
  "y.true" = rowMeans(votediff.y[colnames(votediff.y) %in% colnames(votediff.y.test)][-1], na.rm=TRUE), # nonimputed
  "year" =  votediff.y$year
)

votediff.encoder.decoder$pointwise <- votediff.encoder.decoder$y.true-votediff.encoder.decoder$y.pred

# Post-period MSPE

# Post
votediff.encoder.decoder.MSPE <- filter(votediff.encoder.decoder, year %in% c(2005:2010)) %>% summarise(MSPE=mean((y.true-y.pred)**2))
votediff.encoder.decoder.MSPE

votediff.encoder.decoder.APE <- filter(votediff.encoder.decoder, year %in% c(2005:2010)) %>% mutate(APE=abs(y.pred-y.true)/abs(y.true))
mean(votediff.encoder.decoder.APE$APE)

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

elections.ed.plot <- ggplot(data=votediff.encoder.decoder, aes(x=year)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Winner margin (ln)") + xlab("") +
  geom_vline(xintercept=2005, linetype=2) + 
  ggtitle("Mayoral elections: Encoder-decoder (validation MSPE = 0.521)") +
  theme.blank 

ggsave(paste0(results.directory,"plots/impact-votediff.png"), elections.ed.plot, width=11, height=8.5)