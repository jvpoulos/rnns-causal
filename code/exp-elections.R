###################################
# Two-stage experimental estimates  #
###################################

library(boot)

ads.exp <- fg.ads[!is.na(fg.ads$treat),] #subset to experimental cities 

ads.exp<- ads.exp[c("city","state","year","treat","votediff")]

# 2005 

RunOLS <- function(data, idx) {
  fit <- lm(votediff ~ treat + factor(strata), subset=idx, data)
  coef(fit)
}

votediff.est.2005 <- boot(data.frame(ads.exp[ads.exp$year %in% c(2005),]),
                          RunOLS, 
                          R=1000, 
                          strata=ads.exp[ads.exp$year %in% c(2005),]$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.2005[1]

boot.ci(votediff.est.2005, conf=0.95, index=2, type="basic") # nonparametric bootstrap CIs

votediff.lm.binary.2005 <- lm(votediff ~ treat + factor(strata), data=ads.exp[ads.exp$year %in% c(2005),]) # LM sanity check

summary(votediff.lm.binary.2005)

confint(votediff.lm.binary.2005)[2,]

# 2006
votediff.est.2006 <- boot(data.frame(ads.exp[ads.exp$year %in% c(2006),]),
                          RunTwoStage, 
                          R=1000, 
                          strata=ads.exp[ads.exp$year %in% c(2006),]$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.2006[1]

boot.ci(votediff.est.2006, conf=0.95, index=2, type="basic") # nonparametric bootstrap CIs

# 2005/6 (pooled)
votediff.est.0506 <- boot(data.frame(ads.exp[ads.exp$year %in% c(2005:2006),]),
                          RunTwoStage, 
                          R=1000, 
                          strata=ads.exp[ads.exp$year %in% c(2005:2006),]$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.0506[1]

boot.ci(votediff.est.0506, conf=0.95, index=2, type="basic") # nonparametric bootstrap CIs

# 2005-10 (pooled)
votediff.est.0510 <- boot(data.frame(ads.exp[ads.exp$year %in% c(2005:2010),]),
                          RunTwoStage, 
                          R=1000, 
                          strata=ads.exp[ads.exp$year %in% c(2005:2010),]$strata, # stratify
                          parallel="multicore", ncpus = cores)

votediff.est.0510[1]

boot.ci(votediff.est.0510, conf=0.95, index=2, type="basic") # nonparametric bootstrap CIs