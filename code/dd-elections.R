###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)
library(tidyr)

ads.did <- fg.ads[!is.na(fg.ads$treat),] # subset to experimental cities

ads.did$time <- NA
ads.did$time <- 0
ads.did$time[(ads.did$year >= ads.did$year_exp)] <- 1

# Ensure balanced pre-post sample

ads.did$id <- paste(ads.did$city, ads.did$state, sep=".")

ads.did.means <- ads.did %>% # take city pre/post means
  group_by(id,time) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))  %>%
  select(id, votediff, time, year_exp) 

counts <- ads.did.means %>% 
  group_by(id) %>% 
  summarise(n = n())

ads.did <- ads.did[ads.did$id %in% counts$id[counts$n==2],]

# Create did interaction term

ads.did$did <- NA
ads.did$did <- ads.did$grp_buy* ads.did$time

# DD Estimates (2005 and 2006)

RunDiD <- function(my.data, indices){
  d <- my.data[indices,]
  fit <- lm(y ~ grp_buy + did + factor(year) + factor(strata), data = d)
  return(coef(fit)[['did']])
}

votediff.data <- data.frame(subset(ads.did, !is.na(votediff), select=c('time','grp_buy','did','votediff','strata','year')))
colnames(votediff.data)<- c('time','grp_buy','did','y','strata','year')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$strata, 
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.votediff <- lm(votediff ~ grp_buy + did + factor(year) + factor(strata), data = ads.did) 

summary(did.votediff)

# DD Estimates (2005)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year_exp==2005, select=c('time','grp_buy','did','votediff','strata','year')))
colnames(votediff.data)<- c('time','grp_buy','did','y','strata','year')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (2006)

votediff.data <- data.frame(subset(ads.did, !is.na(votediff) & year_exp==2006, select=c('time','grp_buy','did','votediff','strata','year')))
colnames(votediff.data)<- c('time','grp_buy','did','y','strata','year')

votediff.est <- boot(votediff.data,
                     RunDiD, R=1000, 
                     strata=votediff.data$did, # stratify at time*treat
                     parallel="multicore", ncpus = cores)

votediff.est[1]

boot.ci(votediff.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs