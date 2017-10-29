###################################
# Assemble mayoral elections dataset   #
###################################

library(tidyr)
library(dplyr)
library(caret)
library(weights)

data.directory <- "~/Dropbox/github/rnns-causal/data/"

## Load H&P (2017) data
hopkins <- read.table(paste0(data.directory,"hopkins/mayoralelections_final_full.tab"),
                  header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)

# Extract state abbr

hopkins$state <- str_sub(gsub("\\d+|NA", "", hopkins$citystabbryear),start= -2)

# Clean city names

hopkins$city[hopkins$city=="Louisville-Jefferson County"] <- "Louisville"

# Subset relevant columns
hopkins <- hopkins[c("state","city","year","diffwinnerandloser")]

colnames(hopkins) <- c("state","city","year","votediff")

## Load F&G (2009) data

fg <- readstata13::read.dta13(paste0(data.directory,"ferreira/mayoral_data_4.17.09.dta"))

fg$city <- trimws(fg$city)

# Clean vote totals
fg$vote_total[fg$city=="Eden Prairie" & fg$state=="MN" & fg$year==1982] <- fg$mayor_votes[fg$city=="Eden Prairie" & fg$state=="MN" & fg$year==1982] + 
  fg$runnerup_votes[fg$city=="Eden Prairie" & fg$state=="MN" & fg$year==1982]

fg$vote_total[fg$city=="Beaumont" & fg$state=="TX" & fg$year%in%c(1978,1982)] <- fg$mayor_votes[fg$city=="Beaumont" & fg$state=="TX" & fg$year%in%c(1978,1982)] + 
  fg$runnerup_votes[fg$city=="Beaumont" & fg$state=="TX" & fg$year%in%c(1978,1982)]

fg$vote_total[fg$city=="Superior" & fg$state=="WI" & fg$year%in%c(1975,1983)] <- fg$mayor_votes[fg$city=="Superior" & fg$state=="WI" & fg$year%in%c(1975,1983)] + 
  fg$runnerup_votes[fg$city=="Superior" & fg$state=="WI" & fg$year%in%c(1975,1983)]

fg$vote_total[fg$city=="Medford" & fg$state=="OR" & fg$year==1978] <- fg$mayor_votes[fg$city=="Medford" & fg$state=="OR" & fg$year==1978] + 
  fg$runnerup_votes[fg$city=="Medford" & fg$state=="OR" & fg$year==1978]

fg$vote_total[fg$city=="Warren" & fg$state=="MI" & fg$year==1991] <- fg$mayor_votes[fg$city=="Warren" & fg$state=="MI" & fg$year==1991] + 
  fg$runnerup_votes[fg$city=="Warren" & fg$state=="MI" & fg$year==1991]

# Create vote share measures

fg$votediff <- ((fg$mayor_votes - fg$runnerup_votes)/fg$vote_total)*100 # vote margin separating mayor from the closest challenger

fg$incumbentshare <- (fg$mayor_votes)/fg$vote_total # incumbent vote share

# # Create covariates
# 
# fg.covars <- cbind(fg[c("city","state","year")], dummify(factor(fg$mayor_party, labels=c("Mayor.NA","Mayor.D","Mayor.I","Mayor.O","Mayor.R"))))
# 
# fg.covars <- cbind(fg.covars, dummify(factor(fg$runnerup_party, labels=c("Runnerup.NA","Runnerup.D","Runnerup.I","Runnerup.O","Runnerup.R"))))
# 
# fg.covars <- cbind(fg.covars, dummify(factor(fg$commclass, labels=c("CC.1","CC.2","CC.3","CC.4","CC.5"))))
# 
# fg.covars.names <- colnames(fg.covars)[!colnames(fg.covars) %in% c("city","state","year")]

# Take means for multiple elections in same year

fg <- fg %>% 
  filter(!is.na(votediff)) %>% 
  group_by(state, city, year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))  %>%
  select(state, city, year,votediff)

fg <- data.frame(fg)

## Append mayoral elections

fg.hopkins <- rbind(fg, hopkins[paste(hopkins$city,hopkins$state,hopkins$year) %in%
                                  setdiff(paste(hopkins$city,hopkins$state,hopkins$year),
                                        paste(fg$city,fg$state,fg$year)),])

fg.hopkins <- fg.hopkins[with(fg.hopkins, order(state, city, year)), ] # sort

#fg.hopkins <- merge(fg.hopkins, fg.covars, by=c("state","city","year"), all.x=TRUE)

## Load P&G (2008) data

ads <- read.dta(paste0(data.directory,"pg/PanagopoulosGreen_AJPS_2008_ReplicationDataset.dta"), convert.factors=F)

# Clean city name

ads$city[ads$city==".                     Dayton"] <- "Dayton"
ads$city[ads$city==".               Attleboro"] <- "Attleboro"
ads$city[ads$city==".          Council Bluffs"] <- "Council Bluffs"
ads$city[ads$city=="Louisville Metro"] <- "Louisville"

# Subset

ads$year <- ads$year_exp

ads <- ads[c("state","city","year","votesharediff","year_exp", "strata90","strata70", "grp_buy")]

colnames(ads) <- c("state","city","year","votediff","year_exp", "strata90","strata70", "grp_buy")

## Merge datasets

fg.ads <- merge(fg.hopkins, ads, by=c("state","city","year"), all=TRUE)

# Fill in missing 2005/6 vote differences with P&G data, if available
fg.ads$votediff.x[is.na(fg.ads$votediff.x) & ! is.na(fg.ads$votediff.y)] <- fg.ads$votediff.y[is.na(fg.ads$votediff.x) & ! is.na(fg.ads$votediff.y)]

# Binary treatment
fg.ads$treat <- NA
fg.ads$treat[fg.ads$grp_buy==0] <- 0
fg.ads$treat[fg.ads$grp_buy>0] <- 1

# Fill by city/state
fg.ads <- fg.ads[with(fg.ads, order(state, city, year)), ] # sort

fg.ads <- fg.ads  %>% group_by(city, state) %>% fill(treat, year_exp, grp_buy, strata90, strata70, .direction="down") # fill missing
fg.ads <- fg.ads  %>% group_by(city, state) %>% fill(treat,year_exp, grp_buy, strata90, strata70, .direction="up") # fill missing

# Clean up
fg.ads <- fg.ads[!colnames(fg.ads) %in% c("votediff.y")]
colnames(fg.ads)[colnames(fg.ads)%in%"votediff.x"] <-"votediff"

# Remove if missing votediff

fg.ads <- fg.ads[!is.na(fg.ads$votediff),]

fg.ads <- fg.ads[with(fg.ads, order(state, city, year)), ] # sort

# Price strata 
fg.ads$strata <- NA
fg.ads$strata[fg.ads$strata70==1] <- 70
fg.ads$strata[fg.ads$strata90==1] <- 90
fg.ads$strata[fg.ads$strata70==0 & fg.ads$strata90==0] <- 50

# Create means by treatment status
fg.ads.treat <- fg.ads %>% 
#  filter(!is.na(treat)) %>% # keep experimental cities
  group_by(year,treat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(year, treat, votediff)

fg.ads.control <- fg.ads[fg.ads$treat%in% c(NA,0),][c("year","city","state","votediff")] # discard treated since we have treated time-series

fg.ads.control$id <- paste(fg.ads.control$city, fg.ads.control$state,sep=".")

## Reshape

fg.ads.treat <- reshape(data.frame(fg.ads.treat)[c("year","treat","votediff")], idvar = "year", timevar = "treat", direction = "wide")

votediff <- reshape(data.frame(fg.ads.control)[c("year","id","votediff")], idvar = "year", timevar = "id", direction = "wide")
votediff <- votediff[with(votediff, order(year)), ] # sort

#votediff.names <- grep("votediff", names(votediff), value = TRUE)
#binary.names <- colnames(votediff)[!colnames(votediff)%in%votediff.names]

#Labels

votediff.y <- fg.ads.treat[c("year", "votediff.1")]
votediff.y <- votediff.y[!is.na(votediff.y$votediff.1),]

# Splits

votediff.years <- sort(intersect(votediff$year,votediff.y$year)) # common years in treated and control

votediff.x.train <- votediff[votediff$year %in% votediff.years & votediff$year < 2002,]
votediff.x.val <- votediff[votediff$year %in% votediff.years & (votediff$year >= 2002 & votediff$year < 2005),] # 2002-2004 for validation
votediff.x.test <- votediff[votediff$year %in% votediff.years & votediff$year >= 2005,]

votediff.y.train <- votediff.y[votediff.y$year %in% votediff.years & votediff.y$year < 2002,]
votediff.y.val <- votediff.y[votediff.y$year %in% votediff.years & (votediff.y$year >= 2002 & votediff.y$year < 2005),]
votediff.y.test <- votediff.y[votediff.y$year %in% votediff.years &votediff.y$year >= 2005,]

# Preprocess (only votediff)
#binary.names <- gsub(" ", ".",binary.names) # fix

votediff.x.train <- data.frame(sapply(votediff.x.train, as.numeric))
votediff.pre.train <- preProcess(votediff.x.train[!colnames(votediff.x.train) %in% c("year")], method = c("medianImpute","center","scale"))
votediff.x.train[!colnames(votediff.x.train) %in% c("year")] <- predict(votediff.pre.train, votediff.x.train[!colnames(votediff.x.train) %in% c("year")] )

votediff.x.val <- data.frame(sapply(votediff.x.val, as.numeric))
votediff.x.val[!colnames(votediff.x.val) %in% c("year")] <- predict(votediff.pre.train, votediff.x.val[!colnames(votediff.x.val) %in% c("year")] ) # use training values for val set 

votediff.x.test <- data.frame(sapply(votediff.x.test, as.numeric))
votediff.x.test[!colnames(votediff.x.test) %in% c("year")] <- predict(votediff.pre.train, votediff.x.test[!colnames(votediff.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)

write.csv(votediff.x.train[!colnames(votediff.x.train) %in% c("year")], paste0(data.directory,"elections/treated/votediff-x-train.csv"), row.names=FALSE) 
write.csv(votediff.x.val[!colnames(votediff.x.val) %in% c("year")] , paste0(data.directory,"elections/treated/votediff-x-val.csv"), row.names=FALSE) 
write.csv(votediff.x.test[!colnames(votediff.x.test) %in% c("year")] , paste0(data.directory,"elections/treated/votediff-x-test.csv"), row.names=FALSE) 
write.csv(votediff.y.train[!colnames(votediff.y.train) %in% c("year")], paste0(data.directory,"elections/treated/votediff-y-train.csv"), row.names=FALSE) 
write.csv(votediff.y.val[!colnames(votediff.y.val) %in% c("year")], paste0(data.directory,"elections/treated/votediff-y-val.csv"), row.names=FALSE) 
write.csv(votediff.y.test[!colnames(votediff.y.test) %in% c("year")], paste0(data.directory,"elections/treated/votediff-y-test.csv"), row.names=FALSE) 