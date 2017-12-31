######################################################################
# Synthetic control applied to elections data                         #
######################################################################

library(Synth)
library(plm)
library(dplyr)
library(tidyr)
library(tseries)
library(boot)
library(reshape2)

source(paste0(code.directory,'PolitisWhite.R')) 

# Get splits

x.train <- read.csv(paste0(data.directory,"elections/sim/sim_x_train_treated.csv"), header=FALSE)
x.test <- read.csv(paste0(data.directory,"elections/sim/sim_x_test_treated.csv"), header=FALSE)

y.train <- read.csv(paste0(data.directory,"elections/sim/sim_y_train_treated.csv"), header=FALSE)
y.test <- read.csv(paste0(data.directory,"elections/sim/sim_y_test_treated.csv"), header=FALSE)

phi <- -abs(y.test*0.1)

y.test.c <- y.test + phi  #true counterfactual

# Construct panel

x.sim <- cbind("t" = 1:52,
                   rbind(x.train,x.test))

y.sim <- cbind("t" = 1:52,
               "y" = rowMeans(rbind(y.train,y.test))) 

synth.sim.panel <- cbind(y.sim, x.sim[-1])
  
synth.sim.panel <- melt(synth.sim.panel, id.vars = c("t"))

colnames(synth.sim.panel) <- c("t","id", "y")

# Create numeric id

synth.sim.panel <- transform(synth.sim.panel,num=as.numeric(factor(synth.sim.panel$id)))

synth.sim.panel$id <- as.character(synth.sim.panel$id)

# create matrices from panel data that provide inputs for synth()

dataprep.out<-
  dataprep(
    foo = synth.sim.panel,
    predictors = NULL,
    predictors.op = "mean",
    dependent = "y",
    unit.variable = "num",
    time.variable = "t",
    special.predictors = list(
      list("y",1:15,c("mean")),
      list("y",15:30,c("mean")),
      list("y",30:45,c("mean")),
      list("y",45:52,c("mean")),
      list("y",1:52,c("mean"))),
    treatment.identifier = 1,
    controls.identifier = sort(unique(synth.sim.panel$num[!synth.sim.panel$num %in% c(1)])),
    time.predictors.prior = c(1:47),
    time.optimize.ssr = c(43:47),
    unit.names.variable = "id",
    time.plot = 1:52
  )


## run the synth command to identify the weights
## that create the best possible synthetic
## control unit for the treated.
#synth.out.sim <- synth(dataprep.out)

#saveRDS(synth.out.sim, paste0(data.directory, "synth-out-sim.rds"))

synth.out.sim <- readRDS(paste0(data.directory, "synth-out-sim.rds"))

## there are two ways to summarize the results
## we can either access the output from synth.out.sim.simdirectly
round(synth.out.sim$solution.w,2)

# contains the unit weights or
synth.out.sim$solution.v
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
  dataprep.out$Y0plot%*%synth.out.sim$solution.w
) ; gaps
## also there are three convenience functions to summarize results.
## to get summary tables for all information
## (V and W weights plus balance btw.
## treated and synthetic control) use the
## synth.tab() command
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out.sim)
print(synth.tables)
## to get summary plots for outcome trajectories
## of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands
## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out.sim)
## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out.sim)

# Bootstrap estimate for prediction

bopt <- b.star(dataprep.out$Y0plot%*%synth.out.sim$solution.w, round=TRUE)[[1]]  # get optimal bootstrap block lengths

synth.results <- data.frame('y.true'=dataprep.out$Y1plot[,1],
                            'y.pred'= dataprep.out$Y0plot%*%synth.out.sim$solution.w[,1])
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

synth.plot <- ggplot(data=synth.results, aes(x=1:52)) +
  geom_line(aes(y=y.true, colour = "Observed treated outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted treated outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Outcome") + xlab("") +
  geom_vline(xintercept=48, linetype=2) + 
  geom_ribbon(aes(ymin=y.pred.min, ymax=y.pred.max), fill="grey", alpha=0.5) +
  ggtitle(paste0("Simulated data: Synthetic control (training MSPE = ", round(synth.out.sim$loss.v[[1]],2), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/synth-plot-sim.png"), synth.plot, width=11, height=8.5)

# True Phi

synth.results$y.phi <- NA
synth.results$y.phi[rownames(synth.results) %in% c(48:52)] <- rowMeans(phi)

# Absolute percentage estimation error

sim.APE <- filter(synth.results, rownames(synth.results) %in% c(48:52)) %>% mutate(APE=abs(pointwise-y.phi)/abs(y.phi))
