###################################
# GBR estimates                       #
###################################

library(devtools)

install_github("google/GeoexperimentsResearch")

library(GeoexperimentsResearch)
library(boot)
library(tidyr)
library(zoo)

source(paste0(code.directory,"Run2Stage.R"))

ads.exp <- fg.ads[!is.na(fg.ads$treat),] # subset to experimental cities

# Create var for when treatment started

ads.exp$time <- NA
ads.exp$time <- 0
ads.exp$time[(ads.exp$year == ads.exp$year_exp)] <- 1

# Ensure balanced pre-post sample

ads.exp$id <- paste(ads.exp$city, ads.exp$state, sep=".")

ads.exp.means <- ads.exp %>% # take city pre/post means
  group_by(id,time) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

ads.exp.means <- ads.exp.means[c("id","votediff","year_exp","time")]

counts <- ads.exp.means %>% 
  group_by(id) %>% 
  summarise(n = n())

ads.exp.means <- ads.exp.means[ads.exp.means$id %in% counts$id[counts$n==2],]

# Reshape and include post-period ad buy

ads.exp.means$time <- ads.exp.means$time* ads.exp.means$year_exp

ads.exp.means <- spread(ads.exp.means, key = time, value = votediff)

ads.exp.means <- merge(ads.exp.means, ads.exp[c("id","strata","grp_buy")], by="id", all.x=TRUE)
ads.exp.means <- ads.exp.means[!duplicated(ads.exp.means),]

## 2005

votediff.exp.05 <- data.frame(ads.exp.means[!is.na(ads.exp.means$'2005'),])[-5]

colnames(votediff.exp.05) <- c("id","year_exp","pre","post","strata","grp_buy" )

votediff.est.05 <- boot(votediff.exp.05,
                          Run2Stage, 
                          R=1000,
                          strata=votediff.exp.05$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.05$t0['grp_buy']

boot.ci(votediff.est.05, conf=0.95, index=3, type="basic") # nonparametric bootstrap CIs

## 2006

votediff.exp.06 <- data.frame(ads.exp.means[!is.na(ads.exp.means$'2006'),])[-4]

colnames(votediff.exp.06) <- c("id","year_exp","pre","post","strata","grp_buy" )

votediff.est.06 <- boot(votediff.exp.06,
                        Run2Stage, 
                        R=1000,
                        strata=votediff.exp.06$strata, # stratify
                        parallel="multicore", ncpus = cores)

votediff.est.06$t0['grp_buy']

boot.ci(votediff.est.06, conf=0.95, index=3, type="basic") # nonparametric bootstrap CIs

## 2005-2006 (pooled)

votediff.exp.0506 <- data.frame(ads.exp.means)

votediff.exp.0506$post <- votediff.exp.0506$X2005
votediff.exp.0506$post[is.na(votediff.exp.0506$post)] <- votediff.exp.0506$X2006[is.na(votediff.exp.0506$post)]
votediff.exp.0506$pre <- votediff.exp.0506$X0

votediff.est.0506 <- boot(votediff.exp.0506,
                        Run2Stage, 
                        R=1000,
                        strata=votediff.exp.0506$strata, # stratify
                        parallel="multicore", ncpus = cores)

votediff.est.0506$t0['grp_buy']

boot.ci(votediff.est.0506, conf=0.95, index=3, type="basic") # nonparametric bootstrap CIs