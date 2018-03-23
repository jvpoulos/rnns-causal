###################################
# GBR estimates                       #
###################################

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

colnames(ads.exp.means) <- c("id","year_exp","pre","t05","t06","strata","grp_buy" )

ads.exp.means$strata70 <- ifelse(ads.exp.means$strata==70, 1, 0)
ads.exp.means$strata90 <- ifelse(ads.exp.means$strata==90, 1, 0)
  
## 2005

f1 <- formula(t05 ~ pre + delta + strata70 + strata90, weights=1/pre) # eq. 1 in vaver 2011
f2 <- formula(grp_buy ~ grp_buy)

votediff.est.05 <- boot(data=ads.exp.means,
                 statistic=Run2Stage,
                 f1=f1, f2=f2,
                 first.test=TRUE, # no ad spending in j-1
                 R=1000,
                 strata=ads.exp.means$strata, 
                 parallel="multicore", ncpus = cores)

votediff.est.05.delta <- votediff.est.05$t0[['delta']]
votediff.est.05.delta

votediff.est.05.CI <- boot.ci(votediff.est.05, conf=0.95, index=3, type="basic")$basic[4:5] # 95% nonparametric bootstrap CIs
votediff.est.05.CI

# sanity check

summary(lm(t05 ~ pre + grp_buy + strata70 + strata90, ads.exp.means)) # N=31

## 2006

f1 <- formula(t06 ~ pre + delta + strata70 + strata90, weights=1/pre) # eq. 1 in vaver 2011
f2 <- formula(grp_buy ~ grp_buy)

votediff.est.06 <- boot(data=ads.exp.means,
                        statistic=Run2Stage,
                        f1=f1, f2=f2,
                        first.test=TRUE, # no ad spending in j-1
                        R=1000,
                        strata=ads.exp.means$strata, 
                        parallel="multicore", ncpus = cores)

votediff.est.06.delta <- votediff.est.06$t0[['delta']]
votediff.est.06.delta

votediff.est.06.CI <- boot.ci(votediff.est.06, conf=0.95, index=3, type="basic")$basic[4:5] # 95% nonparametric bootstrap CIs
votediff.est.06.CI

# sanity check

summary(lm(t06 ~ pre + grp_buy + strata70 + strata90, ads.exp.means)) # N=17

## 2005-2006 (pooled)

ads.exp.means$post <- ads.exp.means$t05
ads.exp.means$post[is.na(ads.exp.means$post)] <- ads.exp.means$t06[is.na(ads.exp.means$post)]

ads.exp.means$year06 <- ifelse(ads.exp.means$year_exp==2006,1,0) # year dummy

f1 <- formula(post ~ pre + delta + strata70 + strata90 + year06, weights=1/pre) # eq. 1 in vaver 2011
f2 <- formula(grp_buy ~ grp_buy)

votediff.est.0506 <- boot(data=ads.exp.means,
                        statistic=Run2Stage,
                        f1=f1, f2=f2,
                        first.test=TRUE, # no ad spending in j-1
                        R=1000,
                        strata=ads.exp.means$strata, 
                        parallel="multicore", ncpus = cores)

votediff.est.0506.delta <- votediff.est.0506$t0[['delta']]
votediff.est.0506.delta

votediff.est.0506.CI <- boot.ci(votediff.est.0506, conf=0.95, index=3, type="basic")$basic[4:5] # 95% nonparametric bootstrap CIs
votediff.est.0506.CI

# sanity check

summary(lm(post ~ pre + grp_buy + strata70 + strata90 + year06, ads.exp.means)) # N=48