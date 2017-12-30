######################################################################
# Synthetic control applied to elections data                         #
######################################################################

library(Synth)
library(plm)
library(dplyr)
library(tidyr)
library(tseries)
library(boot)

source(paste0(code.directory,'PolitisWhite.R')) 

## The usual sequence of commands is:
## 1. dataprep() for matrix-extraction
## 2. synth() for the construction of the synthetic control group
## 3. synth.tab(), gaps.plot(), and path.plot() to summarize the results

# Merge covariates back to elections data

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

# ## First Example: Toy panel dataset
# # load data
# data(synth.data)

# create matrices from panel data that provide inputs for synth()

dataprep.out<-
  dataprep(
    foo = fg.ads.synth,
    predictors = c("logvotetotal"),
    predictors.op = "mean",
    dependent = "votediff",
    unit.variable = "num",
    time.variable = "year",
    special.predictors = list(
      list("votediff",1945:1960,c("mean")),
      list("votediff",1960:1975,c("mean")),
      list("votediff",1975:1990,c("mean")),
      list("votediff",1990:2004,c("mean")),
      list("votediff",1945:2004,c("mean"))),
    treatment.identifier = 999,
    controls.identifier = sort(unique(fg.ads.synth$num[!fg.ads.synth$num %in% c(999)])),
    time.predictors.prior = c(1945:2004),
    time.optimize.ssr = c(2000:2004),
    unit.names.variable = "id",
    time.plot = 1980:2006
  )


## run the synth command to identify the weights
## that create the best possible synthetic
## control unit for the treated.
synth.out <- synth(dataprep.out)

saveRDS(synth.out, paste0(data.directory, "synth-out.rds"))

## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)

# contains the unit weights or
synth.out$solution.v
## contains the predictor weights.
## the output from synth opt
## can be flexibly combined with
## the output from dataprep to
## compute other quantities of interest
## for example, the period by period
## discrepancies between the
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps
## also there are three convenience functions to summarize results.
## to get summary tables for all information
## (V and W weights plus balance btw.
## treated and synthetic control) use the
## synth.tab() command
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)
## to get summary plots for outcome trajectories
## of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands
## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)
## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

# Bootstrap estimate for prediction

bopt <- b.star(dataprep.out$Y0plot%*%synth.out$solution.w, round=TRUE)[[1]]  # get optimal bootstrap block lengths

synth.results <- data.frame('y.true'=dataprep.out$Y1plot[,1],
                            'y.pred'= dataprep.out$Y0plot%*%synth.out$solution.w[,1])
# GetPointwise <- function(x){
#    # Calculate pointwise impact
#    return(x['y.true']- x['y.pred'])
# }
#  
# votediff.boot <- tsboot(ts(synth.results), GetPointwise, R = 1000, l = bopt, 
#                          sim = "geom") # block resampling with block lengths having a geometric distribution with mean bopt

votediff.boot <- tsbootstrap(synth.results$y.pred, nb=1000, type="block", b = bopt) # block resampling with block lengths bopt

sd  <- rowSds(votediff.boot) # get Sds

synth.results <- cbind(synth.results, sd)

synth.results$pointwise <- synth.results$y.true- synth.results$y.pred
synth.results$y.pred.min <- synth.results$y.pred-(synth.results$sd*1.96)
synth.results$y.pred.max <- synth.results$y.pred+(synth.results$sd*1.96)
synth.results$pointwise.min <- synth.results$y.true- synth.results$y.pred.max
synth.results$pointwise.max <- synth.results$y.true- synth.results$y.pred.min

# Plot actual versus predicted

theme.blank <- theme(axis.text=element_text(size=12)
                     , axis.title.x=element_blank()
                     , plot.title = element_text(hjust = 0.5)
                     , axis.ticks.x=element_blank()
                     , axis.ticks.y=element_blank()
                     , legend.text=element_text(size=12)
                     , legend.title = element_blank()
                     , legend.position = c(0.25,0.9)
                     , legend.justification = c(1,0))

synth.plot <- ggplot(data=synth.results, aes(x=1980:2006)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Winner margin (%)") + xlab("") +
#  geom_vline(xintercept=2000, linetype=3) + 
  geom_vline(xintercept=2005, linetype=2) + 
  geom_ribbon(aes(ymin=y.pred.min, ymax=y.pred.max), fill="grey", alpha=0.5) +
  ggtitle(paste0("Synthetic control (training MSPE = ", round(synth.out$loss.v[[1]],2), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/synth-plot.png"), synth.plot, width=11, height=8.5)
