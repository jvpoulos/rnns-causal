###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

ads.did<- fg.ads[c("city","state","year","treat","votediff")]

# Create var for when treatment started

ads.did$time <- NA
ads.did$time <- 0
ads.did$time[(ads.did$year >= 2005)] <- 1

ads.did$treat[is.na(ads.did$treat)] <- 0 # untreated cities are control

ads.did$did <- NA
ads.did$did <- ads.did$treat* ads.did$time

# DD Estimates (2005)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year<=2005, select=c('time','treat','did','votediff')))
colnames(votediff.data)<- c('time','treat','did','y')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.votediff <- lm(votediff ~ treat*time, data = ads.did[ads.did$year <=2005,]) 

summary(did.votediff)

confint(did.votediff)[4,]

# DD Estimates (2006)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year%in%c(1947:2004,2006), select=c('time','treat','did','votediff')))
colnames(votediff.data)<- c('time','treat','did','y')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (2005 & 2006)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year%in%c(1947:2006), select=c('time','treat','did','votediff')))
colnames(votediff.data)<- c('time','treat','did','y')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (-2010)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff), select=c('time','treat','did','votediff')))
colnames(votediff.data)<- c('time','treat','did','y')

votediff.est <- boot(votediff.data,
                  RunDiD, R=1000, 
                  strata=votediff.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs