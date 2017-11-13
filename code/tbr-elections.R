###################################
# GBR/TBR estimates                       #
###################################

library(devtools)

install_github("google/GeoexperimentsResearch")

library(GeoexperimentsResearch)
library(boot)
library(tidyr)
library(zoo)

# Put observational data into ts object

ads.obs.tbr <- fg.ads[fg.ads$year>=1948,] # unbalanced sample

ads.obs.tbr$date <-as.Date(as.yearmon(ads.obs.tbr$year) + 11/12, frac = 1) # end of year

ads.obs.tbr$geo <- ads.obs.tbr$id

obj.gts <- GeoTimeseries(ads.obs.tbr, metrics=c("votediff"))

# Define experiment periods
obj.per <- ExperimentPeriods(period.dates=c("1947-12-31", "2005-12-31", "2006-12-31", "2007-12-31", "2010-12-31"),
                             period.names=c("Pretest", "2005","2006","Cooldown"))
obj.per

# Assignment data

obj.ga <- GeoAssignment(data.frame("geo"=ads.obs.tbr$geo[!duplicated(ads.obs.tbr$geo)], 
                                "geo.group"=ads.obs.tbr$treat[!duplicated(ads.obs.tbr$geo)]))

obj.ga$geo.group[is.na(obj.ga$geo.group)] <- 0 #non-treated is control
head(obj.ga)

# Combine experiment data

obj <- GeoExperimentData(obj.gts,
                         periods=obj.per,
                         geo.assignment=obj.ga)
head(obj)

aggregate(obj, by=c('period', 'geo.group')) # check aggregates

# Time-Based Regression (TBR) Analysis

# 2005
tbr.05 <- DoTBRAnalysis(obj, response='votediff',
                        model='tbr1',
                        pretest.period=0,
                        intervention.period=1,
                        cooldown.period=3,
                        control.group=0,
                        treatment.group=1)

tbr.05$y[tbr.05$date=="2005-12-31"] - tbr.05$pred[tbr.05$date=="2005-12-31"]
tbr.05$y[tbr.05$date=="2005-12-31"] - (tbr.05$pred[tbr.05$date=="2005-12-31"] + tbr.05$predsd[tbr.05$date=="2005-12-31"])
tbr.05$y[tbr.05$date=="2005-12-31"] - (tbr.05$pred[tbr.05$date=="2005-12-31"] - tbr.05$predsd[tbr.05$date=="2005-12-31"])

# 2006
tbr.06 <- DoTBRAnalysis(obj, response='votediff',
                        model='tbr1',
                        pretest.period=0,
                        intervention.period=2,
                        cooldown.period=3,
                        control.group=0,
                        treatment.group=1)

tbr.06$y[tbr.06$date=="2006-12-31"] - tbr.06$pred[tbr.06$date=="2006-12-31"]
tbr.06$y[tbr.06$date=="2006-12-31"] - (tbr.06$pred[tbr.06$date=="2006-12-31"] + tbr.06$predsd[tbr.06$date=="2006-12-31"])
tbr.06$y[tbr.06$date=="2006-12-31"] - (tbr.06$pred[tbr.06$date=="2006-12-31"] - tbr.06$predsd[tbr.06$date=="2006-12-31"])

# 2005/6 (pooled)
tbr.0506 <- DoTBRAnalysis(obj, response='votediff',
                          model='tbr1',
                          pretest.period=0,
                          intervention.period=c(1,2),
                          cooldown.period=3,
                          control.group=0,
                          treatment.group=1)

tbr.0506

((tbr.0506$y[tbr.0506$date=="2005-12-31"] - tbr.0506$pred[tbr.0506$date=="2005-12-31"]) + (tbr.0506$y[tbr.0506$date=="2006-12-31"] - tbr.0506$pred[tbr.0506$date=="2006-12-31"]))/2

((tbr.0506$y[tbr.0506$date=="2005-12-31"] - (tbr.0506$pred[tbr.0506$date=="2005-12-31"] + tbr.0506$predsd[tbr.0506$date=="2005-12-31"])) + (tbr.0506$y[tbr.0506$date=="2006-12-31"] - (tbr.0506$pred[tbr.0506$date=="2006-12-31"]+ tbr.0506$predsd[tbr.0506$date=="2006-12-31"])))/2
((tbr.0506$y[tbr.0506$date=="2005-12-31"] - (tbr.0506$pred[tbr.0506$date=="2005-12-31"] - tbr.0506$predsd[tbr.0506$date=="2005-12-31"])) + (tbr.0506$y[tbr.0506$date=="2006-12-31"] - (tbr.0506$pred[tbr.0506$date=="2006-12-31"]- tbr.0506$predsd[tbr.0506$date=="2006-12-31"])))/2

tbr.plot <- plot(tbr.0506) + theme(axis.text=element_text(size=12)
                        , axis.title.x=element_blank()
                        , axis.ticks.x=element_blank()
                        , axis.ticks.y=element_blank())

ggsave(paste0(results.directory,"plots/impact-votediff-tbr.png"), tbr.plot, width=11, height=8.5)
