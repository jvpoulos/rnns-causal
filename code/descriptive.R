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

# By time x region

voteshare.tab.treat <- fg.ads %>%
  filter(!is.na(treat)) %>% # only cities in experiment
  group_by(year,treat) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(year,votediff,incumbentshare,treat)

incumbentshare.treat.time <- ggplot(voteshare.tab.treat, aes( year, incumbentshare*100 ,color=as.factor(treat) )) + 
  geom_smooth(span=0.3) +
   geom_vline(xintercept=2005, linetype=2) + 
   geom_vline(xintercept=2006, linetype=2) + 
  scale_y_continuous(name="Mean winner share (%)") +
  xlab("") +
  scale_color_discrete(name="Treatment status",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Control", "Treated"))

ggsave(paste0(results.directory,"plots/incumbentshare-treat-time.png"), incumbentshare.treat.time, width=11, height=8.5)

votediff.treat.time <- ggplot(voteshare.tab.treat, aes( year, votediff,color=as.factor(treat) )) + 
  geom_smooth(span=0.3) +
  geom_vline(xintercept=2005, linetype=2) + 
  geom_vline(xintercept=2006, linetype=2) + 
  scale_y_continuous(name="Mean winner margin (%)") +
  xlab("") +
  scale_color_discrete(name="Treatment status",
                       #  values=c(Control="red", Treated="blue"),
                       labels=c("Control", "Treated"))

ggsave(paste0(results.directory,"plots/votediff-treat-time.png"), votediff.treat.time, width=11, height=8.5)