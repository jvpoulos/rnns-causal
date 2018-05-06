######################################################################
# Synthetic control applied to votediff data  (Placebo)                       #
######################################################################

library(Synth)
library(plm)
library(dplyr)
library(tidyr)
library(tseries)
library(boot)
library(reshape2)
library(scales)
library(ggplot2)
library(ftsa)

library(foreign)
library(xtable)

library(matrixStats)

# Merge covariates back to votediff data

fg.covars$city <- sub(" ", "", fg.covars$city) # rm space

fg.ads.synth <- merge(fg.ads, fg.covars[c("state","city","year","partisan2","mayor_dem","logvotetotal")], by=c("state","city","year"), all.x=TRUE)

# Create numeric id

fg.ads.synth <- transform(fg.ads.synth,num=as.numeric(factor(fg.ads.synth$id)))

# Take treated means

fg.ads.synth.treat <- fg.ads.synth %>% 
  filter(treat==1) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

fg.ads.synth.treat$num <- 999 # treated id

fg.ads.synth <- subset(fg.ads.synth, treat %in% c(0, NA)) # remove treated units

fg.ads.synth <- rbind(fg.ads.synth, fg.ads.synth.treat)# bind treated mean

fg.ads.synth <- fg.ads.synth[c("num", "id", "year", "votediff", "partisan2","mayor_dem","logvotetotal")]

# Make data balanced 

fg.ads.synth <- fg.ads.synth[!is.na(fg.ads.synth$logvotetotal),] #rm if missing vote total

fg.ads.synth <- make.pbalanced(fg.ads.synth, balance.type="fill", index = c("num", "year"))

fg.ads.synth <- fg.ads.synth %>%
  group_by(num) %>%
  fill(id, votediff, partisan2, mayor_dem, logvotetotal) %>% # fill in id and predictors
  fill(id, votediff, partisan2, mayor_dem, logvotetotal, .direction = "up")

fg.ads.synth <- data.frame(fg.ads.synth[fg.ads.synth$year<=2006,]) # max is 2006 

# run synth on treated and control

RunVotediff<- function(treated.indices){
  # pick v by cross validation
  dataprep.out<-
    dataprep(
      foo = fg.ads.synth[fg.ads.synth$num %in% c(votediff.controls.samp, treated.indices),],
      dependent = "votediff",
      unit.variable = "num",
      time.variable = "year",
      special.predictors = list(
        list("votediff",1989:1993,c("mean")),
        list("votediff",1994:1998,c("mean")),
        list("votediff",1995:1999,c("mean")),
        list("votediff",2000:2004,c("mean"))),
      treatment.identifier = treated.indices,
      controls.identifier = votediff.controls.samp[!votediff.controls.samp %in% treated.indices],
      time.predictors.prior = c(1945:2004),
      time.optimize.ssr = c(2002:2004),
      unit.names.variable = "id",
      time.plot = 1980:2006
    )
  
  synth.out.votediff.cv <- synth(
    data.prep.obj = dataprep.out,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )
  
  dataprep.out<-
    dataprep(
      foo = fg.ads.synth[fg.ads.synth$num %in% c(votediff.controls.samp, treated.indices),],
      dependent = "votediff",
      unit.variable = "num",
      time.variable = "year",
      special.predictors = list(
        list("votediff",1989:1993,c("mean")),
        list("votediff",1994:1998,c("mean")),
        list("votediff",1995:1999,c("mean")),
        list("votediff",2000:2004,c("mean"))),
      treatment.identifier = treated.indices,
      controls.identifier = votediff.controls.samp[!votediff.controls.samp %in% treated.indices],
      time.predictors.prior = c(1945:2004),
      time.optimize.ssr = c(2000:2004),
      unit.names.variable = "id",
      time.plot = 1980:2006
    )
  
  synth.out.votediff <- synth(
    data.prep.obj = dataprep.out,
    custom.v=as.numeric(synth.out.votediff.cv$solution.v)
  )
  
  synth.results <- data.frame('y.true'=dataprep.out$Y1plot[,1],
                              'y.pred'= dataprep.out$Y0plot%*%synth.out.votediff$solution.w[,1])
  return(synth.results)
}

# Run for all possible controls 

votediff.controls <- unique(fg.ads.synth$num[! fg.ads.synth$num%in% fg.ads.synth.treat$num])
votediff.controls.samp <- sample(votediff.controls,ceiling(length(votediff.controls)*0.05)) # randomly sample 5% of controls
  
synth.results.controls <- lapply(votediff.controls.samp, RunVotediff)

synth.votediff.controls.preds <- sapply(synth.results.controls, '[[', 2)

# Run for treated

synth.results.treat <- lapply(unique(fg.ads.synth.treat$num), RunVotediff) # treat num 999

synth.votediff.treat.preds <- sapply(synth.results.treat, '[[', 2)

# Post-period MSE (all controls)

votediff.x.synth <- fg.ads.synth[fg.ads.synth$num %in% c(votediff.controls.samp),]
votediff.x.synth <- reshape(data.frame(votediff.x.synth)[c("year","id","votediff")], idvar = "year", timevar = "id", direction = "wide")
votediff.x.synth <- votediff.x.synth[!colnames(votediff.x.synth) %in% "year"]

votediff.control.forecast <- as.matrix(synth.votediff.controls.preds[(nrow(synth.votediff.controls.preds)-1):nrow(synth.votediff.controls.preds),])
votediff.control.true <- as.matrix(votediff.x.synth[(nrow(votediff.x.synth)-1):nrow(votediff.x.synth),])

votediff.synth.mse <- error(forecast=votediff.control.forecast, true=votediff.control.true, method = "mse") # post-intervention MSE
votediff.synth.mse

# Calculate real treated pooled intervention effect

votediff.treat.forecast <- as.matrix(synth.votediff.treat.preds)[26:27,]

votediff.treat.true <- as.matrix(rowMeans(votediff.y.imp[-1]))[51:52,]

votediff.t.stat <- votediff.treat.true-votediff.treat.forecast # real t stat
votediff.t.stat[1:2] # 2005/2006
mean(votediff.t.stat[1:2]) # pooled

(-0.001 - votediff.t.stat[1])**2 # MSPE 2005
(-0.005 - votediff.t.stat[2])**2 # MSPE 2006
(0.0005 - mean(votediff.t.stat[1:2]))**2 # MSPE pooled

# P-values for both treated and placebo treated

votediff.n.placebo <- ncol(votediff.x.synth)

votediff.p.values.treated <- PermutationTest(votediff.control.forecast, votediff.control.true, votediff.t.stat, votediff.n.placebo,np=10000)

votediff.p.values.control <- sapply(1:length(votediff.controls.samp), function(c){
  votediff.t.stat.control <- rowMeans(as.matrix(votediff.control.true[,c])-as.matrix(votediff.control.forecast[,c]))
  PermutationTest(votediff.control.forecast[,-c], votediff.control.true[,-c], votediff.t.stat.control, votediff.n.placebo-1,np=10000)
})

synth.votediff.fpr <- sum(votediff.p.values.control <=0.05)/length(votediff.p.values.control) #FPR
synth.votediff.fpr

# CIs for treated

votediff.CI.treated <- PermutationCI(votediff.control.forecast, votediff.control.true, votediff.t.stat, votediff.n.placebo, np=10000, l=1000)
votediff.CI.treated[1:2,]

colMeans(votediff.CI.treated[1:2,]) # pooled