######################################################################
# Synthetic control applied to basque data  (Placebo)                       #
######################################################################

library(Synth)
library(plm)
library(dplyr)
library(tidyr)
library(tseries)
library(boot)
library(reshape2)

source(paste0(code.directory,'PolitisWhite.R')) 

## Second example: The economic impact of terrorism in the
## Basque country using data from Abadie and Gardeazabal (2003)
## see JSS paper in the references details
data(basque)

basque <- basque[!basque$regionno%in%c(1,17),] # rm Spain and Basque (treated)

basque.controls <- c(2:16,18)

set.seed(81)
basque.treat <- 10 # catalonia 

basque.controls <- basque.controls[!basque.controls %in% basque.treat]

# reshape and split for BSTS and encoder-decoder

basque.x <- reshape(data.frame(basque[basque$regionno%in%basque.controls,])[c("year","regionno","gdpcap")], idvar = "year", timevar = "regionno", direction = "wide")

basque.y <- reshape(data.frame(basque[basque$regionno%in%basque.treat,])[c("year","regionno","gdpcap")], idvar = "year", timevar = "regionno", direction = "wide")

basque.x.train <- basque.x[basque.x$year < 1970,]
basque.x.test <- basque.x[basque.x$year >= 1970,]

basque.y.train <- basque.y[basque.y$year < 1970,]
basque.y.test <- basque.y[basque.y$year >= 1970,]

write.csv(basque.x.train[!colnames(basque.x.train) %in% c("year")], paste0(data.directory,"basque/treated/basque-x-train.csv"), row.names=FALSE) 
write.csv(basque.x.test[!colnames(basque.x.test) %in% c("year")] , paste0(data.directory,"basque/treated/basque-x-test.csv"), row.names=FALSE) 

write.csv(basque.y.train[!colnames(basque.y.train) %in% c("year")], paste0(data.directory,"basque/treated/basque-y-train.csv"), row.names=FALSE) 
write.csv(basque.y.test[!colnames(basque.y.test) %in% c("year")], paste0(data.directory,"basque/treated/basque-y-test.csv"), row.names=FALSE) 

# dataprep: prepare data for synth
dataprep.out <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier = basque.treat
    ,controls.identifier = basque.controls
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr = c(1960:1969)
    ,unit.names.variable = c("regionname")
    ,time.plot = c(1955:1997)
  )
# 1. combine highest and second highest
# schooling category and eliminate highest category
dataprep.out$X1["school.high",] <-
  dataprep.out$X1["school.high",] +
  dataprep.out$X1["school.post.high",]
dataprep.out$X1 <-
  as.matrix(dataprep.out$X1[
    -which(rownames(dataprep.out$X1)=="school.post.high"),])
dataprep.out$X0["school.high",] <-
  dataprep.out$X0["school.high",] +
  dataprep.out$X0["school.post.high",]
dataprep.out$X0 <-
  dataprep.out$X0[
    -which(rownames(dataprep.out$X0)=="school.post.high"),]
# 2. make total and compute shares for the schooling catgeories
lowest <- which(rownames(dataprep.out$X0)=="school.illit")
highest <- which(rownames(dataprep.out$X0)=="school.high")
dataprep.out$X1[lowest:highest,] <-
  (100 * dataprep.out$X1[lowest:highest,]) /
  sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <-
  100 * scale(dataprep.out$X0[lowest:highest,],
              center=FALSE,
              scale=colSums(dataprep.out$X0[lowest:highest,])
  )
# run synth
# synth.out.basque <- synth(data.prep.obj = dataprep.out)
# 
# saveRDS(synth.out.basque, paste0(data.directory, "synth-out-basque.rds"))

synth.out.basque<- readRDS(paste0(data.directory, "synth-out-basque.rds"))

# Get result tables
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out.basque
)
# results tables:
print(synth.tables)
# plot results:
# path
path.plot(synth.res = synth.out.basque,
          dataprep.res = dataprep.out,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"),
          Ylim = c(0,13),
          Legend = c("Basque country","synthetic Basque country"),
)
## gaps
gaps.plot(synth.res = synth.out.basque,
          dataprep.res = dataprep.out,
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"),
          Ylim = c(-1.5,1.5),
)

# Bootstrap estimate for prediction

bopt <- b.star(dataprep.out$Y0plot%*%synth.out.basque$solution.w, round=TRUE)[[1]]  # get optimal bootstrap block lengths

synth.results <- data.frame('y.true'=dataprep.out$Y1plot[,1],
                            'y.pred'= dataprep.out$Y0plot%*%synth.out.basque$solution.w[,1])

boot <- tsbootstrap(synth.results$y.pred, nb=1000, type="block", b = bopt) # block resampling with block lengths bopt

sd  <- rowSds(boot) # get Sds

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

synth.plot <- ggplot(data=synth.results, aes(x=1955:1997)) +
  geom_line(aes(y=y.true, colour = "Observed placebo outcome"), size=1.2) +
  geom_line(aes(y=y.pred, colour = "Predicted placebo outcome"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("Real per-capita GDP (1986 USD, thousand)") + xlab("") +
  geom_vline(xintercept=1970, linetype=2) + 
  geom_ribbon(aes(ymin=y.pred.min, ymax=y.pred.max), fill="grey", alpha=0.5) +
  ggtitle(paste0("Basque Country data: Synthetic control (training MSPE = ", round(synth.out.basque$loss.v[[1]],4), ")")) +
  theme.blank 

ggsave(paste0(results.directory,"plots/synth-plot-basque.png"), synth.plot, width=11, height=8.5)

# Post-period MSPE

basque.synth.MSPE <- filter(synth.results, rownames(synth.results) %in% c(1970:1997)) %>% mutate(MSPE=mean((y.true-y.pred )**2))
