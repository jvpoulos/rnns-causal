###################################
# Two-stage experimental estimates  #
###################################

library(boot)
library(tidyr)

ads.exp <- fg.ads[fg.ads$year<=2007 & !is.na(fg.ads$treat),] #subset to experimental cities 

ads.exp<- ads.exp[c("city","state","year","treat","strata","votediff","grp_buy")]

# Create var for when treatment started

ads.exp$time <- NA
ads.exp$time <- 0
ads.exp$time[(ads.exp$year >= 2005)] <- 1

ads.exp <- ads.exp[with(ads.exp, order(time, year, city, state)), ] # sort

## 2005-2007 (pooled)

# Create balance pre-post sample

ads.exp.means <- ads.exp %>% # take city pre/post means
  group_by(state, city,time) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))  %>%
  select(state, city,votediff, time) 

counts <- ads.exp.means %>% 
  group_by(state, city) %>% 
  summarise(n = n())

ads.exp.means <- ads.exp.means[ads.exp.means$city %in% counts$city[counts$n==2],]

# Reshape and include post-period ad buy

ads.exp.means <- spread(ads.exp.means, key = time, value = votediff)

colnames(ads.exp.means) <- c( "state", "city",  "pre","post")

ads.exp.means <- merge(ads.exp.means, ads.exp[c("state","city","strata","grp_buy")], by=c("state","city"), all.x=TRUE)
ads.exp.means <- ads.exp.means[!duplicated(ads.exp.means[c("state","city")]),]

Run2Stage <- function(data, indices) {
  d <- data[indices,]
  fit <- lm(post ~ pre + grp_buy + factor(strata), weights=1/post, d)
  coef(fit)
}

votediff.est.0507 <- boot(data.frame(ads.exp.means),
                          Run2Stage, 
                          R=1000, 
                          strata=ads.exp.means$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.0507$t0['grp_buy']

boot.ci(votediff.est.0507, conf=0.95, index=3, type="basic") # nonparametric bootstrap CIs

## 2005-2006 (pooled)

ads.exp.means <- ads.exp[ads.exp$year<=2006,] %>% # take city pre/post means
  group_by(state, city,time) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))  %>%
  select(state, city,votediff, time) 

counts <- ads.exp.means %>% 
  group_by(state, city) %>% 
  summarise(n = n())

ads.exp.means <- ads.exp.means[ads.exp.means$city %in% counts$city[counts$n==2],]

# Reshape and include post-period ad buy

ads.exp.means <- spread(ads.exp.means, key = time, value = votediff)

colnames(ads.exp.means) <- c( "state", "city",  "pre","post")

ads.exp.means <- merge(ads.exp.means, ads.exp[c("state","city","strata","grp_buy")], by=c("state","city"), all.x=TRUE)
ads.exp.means <- ads.exp.means[!duplicated(ads.exp.means[c("state","city")]),]

votediff.est.0506 <- boot(data.frame(ads.exp.means),
                          Run2Stage, 
                          R=1000, 
                          strata=ads.exp.means$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.0506$t0['grp_buy']

boot.ci(votediff.est.0506, conf=0.95, index=3, type="basic") # nonparametric bootstrap CIs

## 2005,2006,2007

for (year in c(2005,2006,2007)) {
  
  ads.exp.means <- ads.exp[ads.exp$year %in% c(1947:2005, year),] %>% # take city pre/post means
  group_by(state, city,time) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))  %>%
  select(state, city,votediff, time) 

counts <- ads.exp.means %>% 
  group_by(state, city) %>% 
  summarise(n = n())

ads.exp.means <- ads.exp.means[ads.exp.means$city %in% counts$city[counts$n==2],]

# Reshape and include post-period ad buy

ads.exp.means <- spread(ads.exp.means, key = time, value = votediff)

colnames(ads.exp.means) <- c( "state", "city",  "pre","post")

ads.exp.means <- merge(ads.exp.means, ads.exp[c("state","city","strata","grp_buy")], by=c("state","city"), all.x=TRUE)
ads.exp.means <- ads.exp.means[!duplicated(ads.exp.means[c("state","city")]),]

print(year)

votediff.est <- boot(data.frame(ads.exp.means),
                          Run2Stage, 
                          R=1000, 
                          strata=ads.exp.means$strata, # stratify
                          parallel="multicore", ncpus = cores)

print(votediff.est$t0['grp_buy'])

print(boot.ci(votediff.est, conf=0.95, index=3, type="basic")) # nonparametric bootstrap CIs
}

