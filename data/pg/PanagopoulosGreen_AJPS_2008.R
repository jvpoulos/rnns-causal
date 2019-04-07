#Analysis of Mayors 2005-2006 experiment: Panagopoulos and Green, 3 March, 2006

#Load data
library(foreign)
setwd("/Users/jason/Dropbox/github/rnns-causal/data/pg")
data <- read.dta("PanagopoulosGreen_AJPS_2008_ReplicationDataset.dta", convert.factors=F)

#Recode data
data$year <- data$year_exp
data$dummy2006 <- data$year-2005
data$x70 <- data$strata70 * data$dummy2006
data$x90 <- data$strata90 * data$dummy2006

#####################
#Randomization check#
#####################
library(car)

rand.1 <- lm(grp_buy ~ strata70 + strata90 + dummy2006, subset=contested_2006==1 & inc_running_2006==1, data=data)

rand.2 <- lm(grp_buy ~ strata70 + strata90 + x70 + x90 + dummy2006 + inc_voteshare_prev + partisan + to_prev + statewide_2005, subset=contested_2006==1 & inc_running_2006==1, data=data)

rand.3 <- lm(grp_buy ~ strata70 + strata90 + dummy2006 + inc_voteshare_prev + partisan + to_prev + statewide_2005, subset=contested_2006==1 & inc_running_2006==1, data=data)
linearHypothesis(rand.3, names(coef(rand.3))[5:8])

################################
#Analyze Incumbents' vote share#
################################
inc.strata.2005 <- lm(votesharechange ~ grp_buy + strata70 + strata90, subset=year==2005 & contested_2006==1, data=data)

inc.cov.2005 <- lm(votesharechange ~ grp_buy + strata70 + strata90 + partisan + to_prev + statewide_2005, subset = year==2005 & contested_2006==1, data=data)

inc.strata.2006 <- lm(votesharechange ~ grp_buy + strata70 + strata90, subset=year==2006 & contested_2006==1, data=data)

inc.cov.2006 <- lm(votesharechange ~ grp_buy + strata70 + strata90 + partisan + to_prev + statewide_2005, subset = year==2006 & contested_2006==1, data=data)

inc.strata.pooled <- lm(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006, subset=contested_2006==1, data=data)

inc.cov.pooled <- lm(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006 + partisan + to_prev + statewide_2005, subset = contested_2006==1, data=data)

#Combine results to create Table 2
results <- list(inc.strata.2005, inc.cov.2005, inc.strata.2006, inc.cov.2006, inc.strata.pooled, inc.cov.pooled)

var.names <- c("grp_buy", "to_prev", "partisan", "statewide_2005", "strata70", "strata90", "dummy2006")

for (i in var.names){
	out <- unlist(lapply(lapply(results, function(x) summary(x)$coefficients[grep(i, names(coef(x))),1:2]), function(x) paste(round(x[1],3)," (",round(x[2],3),")",sep="")))
	eval(parse(text = paste("row.",i, " <- out", sep="")))
}

eval(parse(text = paste("table <- rbind(",paste("row.",var.names,sep="",collapse=", "), ", unlist(lapply(results, function(x) dim(x$model)[1])), unlist(lapply(results, function(x) round(summary(x)$sigma, 2))), unlist(lapply(results, function(x) round(summary(x)$r.squared, 2))))", sep="")))

TABLE.2 <- data.frame(table, row.names=c(var.names,"N", "RMSE", "R^2"))
names(TABLE.2) <- c("2005.strata", "2005.cov", "2006.strata", "2006.cov", "POOLED.strata", "POOLED.cov")

TABLE.2


#Bootstrap incumbrent voteshare
data.subset <- subset(data, contested_2006==1)

#Pooled data, strata
#number of iterations
n <- 100000
bs.strata.pooled <- matrix(NA, nrow=n, ncol=2)

for (i in 1:n){
	temp <- data.subset[sample(1:dim(data.subset)[1],replace=T),]
	out <- lm(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006, subset=contested_2006==1, data=temp)
	
	bs.strata.pooled[i,] <- summary(out)$coefficients[2,1:2]	
}
data.frame(Tr.bs=mean(bs.strata.pooled[,1]), bs.SE=sd(bs.strata.pooled[,1]), pct.Tr.neg = sum(bs.strata.pooled[,1]<0)/n)


#Pooled data, covariates
#number of iterations
n <- 10000
bs.cov.pooled <- matrix(NA, nrow=n, ncol=2)

for (i in 1:n){
	temp <- data.subset[sample(1:dim(data.subset)[1],replace=T),]
	out <- lm(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006 + partisan + to_prev + statewide_2005, subset = contested_2006==1, data=temp)
	
	bs.cov.pooled[n,] <- summary(out)$coefficients[2,1:2]
}
data.frame(Tr.bs=mean(bs.cov.pooled[,1]), bs.SE=sd(bs.cov.pooled[,1]), pct.Tr.neg = sum(bs.cov.pooled[,1]<0)/n)


#IRLS Results
#The results given in the paper were obtained using STATA. R uses a different algorithm for IRLS so the results reported below differ from those in the paper, but not substantively.
library(MASS)
#IRLS pooled, strata
summary(rlm(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006, subset=contested_2006==1, data=data, psi=psi.huber))

#IRLS pooled, covariates
summary(rlm(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006 + partisan + to_prev + statewide_2005, subset = contested_2006==1, data=data, psi=psi.huber))


#Quantile regression results
library(quantreg)

qr.1 <- rq(votesharechange ~ grp_buy + strata70 + strata90 + dummy2006, tau=.5,data=data, subset=contested_2006==1)

boot.qr.1 <- boot.rq(x=qr.1$x, y=qr.1$y, tau=.5, R=1000)

#Print results
data.frame(mean=mean(boot.qr.1[,2]), bs.SE=sd(boot.qr.1[,2]))


#################################
#Analyze Incumbents' vote margin#
#################################
inc.strata.2005 <- lm(votesharediff ~ grp_buy + strata70 + strata90, subset=year==2005 & contested_2006==1 & inc_running_2006==1, data=data)

inc.cov.2005 <- lm(votesharediff ~ grp_buy + strata70 + strata90 + inc_voteshare_prev + partisan + to_prev + statewide_2005, subset = year==2005 & contested_2006==1 & inc_running_2006==1, data=data)

inc.strata.2006 <- lm(votesharediff ~ grp_buy + strata70 + strata90, subset=year==2006 & contested_2006==1 & inc_running_2006==1, data=data)

inc.cov.2006 <- lm(votesharediff ~ grp_buy + strata70 + strata90 + inc_voteshare_prev + partisan + to_prev + statewide_2005, subset = year==2006 & contested_2006==1 & inc_running_2006==1, data=data)

inc.strata.pooled <- lm(votesharediff ~ grp_buy + strata70 + strata90 + dummy2006, subset=contested_2006==1 & inc_running_2006==1, data=data)

inc.cov.pooled <- lm(votesharediff ~ grp_buy + strata70 + strata90 + dummy2006 + inc_voteshare_prev + partisan + to_prev + statewide_2005, subset = contested_2006==1 & inc_running_2006==1, data=data)


#Combine results to create Appendix Table 2
results <- list(inc.strata.2005, inc.cov.2005, inc.strata.2006, inc.cov.2006, inc.strata.pooled, inc.cov.pooled)

var.names <- c("grp_buy", "to_prev", "partisan", "statewide_2005", "strata70", "strata90", "inc_voteshare_prev", "dummy2006")


for (i in var.names){
	
	out <- unlist(lapply(lapply(results, function(x) summary(x)$coefficients[grep(i, names(coef(x))),1:2]), function(x) paste(round(x[1],3)," (",round(x[2],3),")",sep="")))
	
	eval(parse(text = paste("row.",i, " <- out", sep="")))
	
}

eval(parse(text = paste("table <- rbind(",paste("row.",var.names,sep="",collapse=", "), ", unlist(lapply(results, function(x) dim(x$model)[1])), unlist(lapply(results, function(x) round(summary(x)$sigma, 2))), unlist(lapply(results, function(x) round(summary(x)$r.squared, 2))))", sep="")))

APPENDIX.TABLE.2 <- data.frame(table, row.names=c(var.names,"N", "RMSE", "R^2"))
names(APPENDIX.TABLE.2) <- c("2005.strata", "2005.cov", "2006.strata", "2006.cov", "POOLED.strata", "POOLED.cov")

APPENDIX.TABLE.2




