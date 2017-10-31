###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)
library(tidyr)

ads.did <- fg.ads[!is.na(fg.ads$treat),] #subset to experimental cities 

ads.did<- ads.did[fg.ads$year %in% c(1948:2007),][c("city","state","year","treat","strata","votediff")]

# Create var for when treatment started

ads.did$time <- NA
ads.did$time <- 0
ads.did$time[(ads.did$year >= 2005)] <- 1

ads.did$did <- NA
ads.did$did <- ads.did$treat* ads.did$time

# Create balanced sample pre/post

ads.did$id <- paste(ads.did$city, ads.did$state, sep=",")

ads.did.means <- ads.did %>% # take city pre/post means
  group_by(id,time) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))  %>%
  select(id,votediff, time) 

counts <- ads.did.means %>% 
  group_by(id) %>% 
  summarise(n = n())

ads.did <- ads.did[ads.did$id %in% counts$id[counts$n==2],]

# Fill strata by city/state
ads.did <- ads.did[with(ads.did, order(state, city, year)), ] # sort

ads.did <- ads.did  %>% group_by(city, state) %>% fill(strata, .direction="down") # fill missing
ads.did <- ads.did  %>% group_by(city, state) %>% fill(strata, .direction="up") # fill missing

# DD Estimates (2005)

RunDiD <- function(my.data, indices){
  d <- my.data[indices,]
  fit <- lm(y ~ treat*time + factor(strata), data = d)
  return(coef(fit)[['treat:time']])
}

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year<=2005, select=c('time','treat','did','votediff','strata')))
colnames(votediff.data)<- c('time','treat','did','y','strata')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$strata, 
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.votediff <- lm(votediff ~ treat*time + factor(strata), data = ads.did[ads.did$year <=2005,]) 

summary(did.votediff)

# DD Estimates (2006)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year%in%c(1948:2004,2006), select=c('time','treat','did','votediff','strata')))
colnames(votediff.data)<- c('time','treat','did','y','strata')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (2007)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year%in%c(1948:2004,2007), select=c('time','treat','did','votediff','strata')))
colnames(votediff.data)<- c('time','treat','did','y','strata')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (2005 & 2006)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year%in%c(1948:2006), select=c('time','treat','did','votediff','strata')))
colnames(votediff.data)<- c('time','treat','did','y','strata')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (2005, 2006, 2007)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year%in%c(1948:2007), select=c('time','treat','did','votediff','strata')))
colnames(votediff.data)<- c('time','treat','did','y','strata')

votediff.est <- boot(votediff.data,
                  RunDiD, R=1000, 
                  strata=votediff.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs