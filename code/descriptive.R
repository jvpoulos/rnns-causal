#####################################
### Descriptive statistics         ###
#####################################

library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)

## Plot vote measure time-series

# By time

voteshare.tab <- fg.ads %>%
  group_by(year) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(year,votediff,incumbentshare)

voteshare.time <- ggplot(voteshare.tab, aes(x=year,y=votediff)) + 
  geom_smooth(aes(colour='votediff'),span=0.25) +
  geom_smooth(aes(y=incumbentshare*100,colour='incumbentshare'),span=0.25) +
  scale_y_continuous(name="Mean vote measure (%)") +
  xlab("") +
  scale_colour_manual(name="Vote measure",
                      values=c(votediff="red", incumbentshare="blue"),
                      labels=c('Winner share', 'Winner margin'))

ggsave(paste0(results.directory,"plots/voteshare-time.png"), voteshare.time, width=11, height=8.5)

# By time x price strata

voteshare.tab.strata <- fg.ads %>%
  filter(!is.na(strata)) %>% # only cities in experiment
  group_by(year,strata) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(year,votediff,incumbentshare,strata)

incumbentshare.strata.time <- ggplot(voteshare.tab.strata, aes( year, incumbentshare*100 ,color=as.factor(strata) )) + 
  geom_smooth(span=0.3) +
  scale_y_continuous(name="Mean winner share (%)") +
  xlab("") +
  scale_color_discrete(name="Price strata")

ggsave(paste0(results.directory,"plots/incumbentshare-strata-time.png"), incumbentshare.strata.time, width=11, height=8.5)

votediff.strata.time <- ggplot(voteshare.tab.strata, aes( year, votediff ,color=as.factor(strata) )) + 
  geom_smooth(span=0.3) +
  scale_y_continuous(name="Mean winner share (%)") +
  xlab("") +
  scale_color_discrete(name="Price strata")

ggsave(paste0(results.directory,"plots/votediff-strata-time.png"), votediff.strata.time, width=11, height=8.5)