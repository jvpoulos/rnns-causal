# Plot time-series and estimate causal impacts
# Uses train/test sets from elections.R

require(reshape2)
require(dplyr)
require(zoo)
require(matrixStats)

source(paste0(code.directory,"ts-plot-elections.R"))

code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

pre.period <- 47

test.features <- ncol(votediff.y.test)-1

analysis <- "auto"

# Import test results

if(analysis=="supervised"){
  setwd(paste0(results.directory, "elections/votediff")) # prediction files loc
  best.model <- 17
  }

if(analysis=="auto"){
  setwd(paste0(results.directory, "elections/votediff-auto")) # prediction files loc
  best.model <- 11
  }

test.files <- list.files(pattern = "*test.csv")#[!list.files(pattern = "*test.csv") %in% burn.files]

votediff.preds.test <- lapply(test.files,function(x){
  m <- read.csv(x, header=FALSE)
  return(as.matrix(m))})

votediff.preds.test.sd <- apply(simplify2array(votediff.preds.test), 1:2, sd)

#votediff.preds.test.mean <- apply(simplify2array(votediff.preds.test), 1:2, mean) # element-wise mean
votediff.preds.test.mean <- votediff.preds.test[[best.model]] # best model

colnames(votediff.preds.test.mean) <- colnames(votediff.y.test)[-1]

# Bind predictions

votediff.bind.preds <-  data.frame(y.pred=rbind(matrix(data=NA,pre.period,test.features),votediff.preds.test.mean))

votediff.bind.sds <-  data.frame(y.sd=rbind(matrix(data=NA,pre.period,test.features),votediff.preds.test.sd))

votediff.bind.true <-  data.frame(y.true=votediff.y[colnames(votediff.y) %in% colnames(votediff.y.test)][-1]) # nonimputed

## Create time series data
setwd(code.directory)

## Plot time series 

# # Adjust year for plot
votediff.bind.year <- as.Date(as.yearmon(votediff.y$year) + 11/12, frac = 1) # end of year

votediff.bind.year <- as.POSIXct(votediff.bind.year, tz="UTC")

# Combine /take means across features

votediff.bind.elections <- data.frame("year"=votediff.bind.year,
                                "y.pred"=rowMeans(votediff.bind.preds), 
                                "y.true"=rowMeans(votediff.bind.true, na.rm = TRUE),
                                "y.sd"=rowMeans(votediff.bind.sds, na.rm = TRUE))

votediff.bind.elections <- votediff.bind.elections  %>%
  mutate(pred.votediff.min = y.pred - y.sd*1.96,
         pred.votediff.max = y.pred + y.sd*1.96,
         pointwise.votediff = y.true - y.pred,
         pointwise.votediff.min = y.true-pred.votediff.max,
         pointwise.votediff.max = y.true-pred.votediff.min)

if(analysis=="supervised"){
  main <- "Encoder-decoder (+ dense output)"
  ts.plot <- TsPlotElections(votediff.bind.elections[votediff.bind.elections$year>="1979-12-30 19:00:00",], main=main)
  ggsave(paste0(results.directory,"plots/impact-votediff.png"), ts.plot, width=11, height=8.5)
}

if(analysis=="auto"){
  main <- "Encoder-decoder"
  ts.plot <- TsPlotElections(votediff.bind.elections[votediff.bind.elections$year>="1979-12-30 19:00:00",], main=main)
  ggsave(paste0(results.directory,"plots/impact-votediff-auto.png"), ts.plot, width=11, height=8.5)
}