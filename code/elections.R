###################################
# Assemble mayoral elections dataset   #
###################################

library(tidyr)
library(dplyr)
library(caret)
library(weights)
library(stringr)
library(foreign)
library(imputeTS)

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

# Take means for multiple elections in same year

fg <- fg %>% 
  filter(!is.na(votediff)) %>% 
  group_by(state, city, year) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

fg <- data.frame(fg[c("state", "city", "year","votediff")])

## Append mayoral elections

fg.hopkins <- rbind(fg, hopkins[paste(hopkins$city,hopkins$state,hopkins$year) %in%
                                  setdiff(paste(hopkins$city,hopkins$state,hopkins$year),
                                        paste(fg$city,fg$state,fg$year)),])

fg.hopkins <- fg.hopkins[with(fg.hopkins, order(state, city, year)), ] # sort

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

fg.ads$city <- gsub("[^[:alnum:] ]", "",fg.ads$city) # clean name 
fg.ads$city <- gsub(" ", "",fg.ads$city)  

# Subset to cities with positive, non-missing vote difference

fg.ads <- fg.ads[!is.na(fg.ads$votediff) & fg.ads$votediff>0,]

fg.ads <- fg.ads[with(fg.ads, order(state, city, year)), ] # sort

# Price strata 
fg.ads$strata <- NA
fg.ads$strata[fg.ads$strata70==1] <- 70
fg.ads$strata[fg.ads$strata90==1] <- 90
fg.ads$strata[fg.ads$strata70==0 & fg.ads$strata90==0] <- 50

fg.ads$id <- paste(fg.ads$city, fg.ads$state,sep=".")

# Create means by treatment status

fg.ads.treat <- spread(subset(fg.ads, treat==1, select=c("id","year","votediff")), key = id, value = votediff)

fg.ads.control <- subset(fg.ads, treat %in% c(0,NA), select=c("id","year","votediff")) # discard treated since we have treated time-series

# Reshape

votediff <- reshape(data.frame(fg.ads.control)[c("year","id","votediff")], idvar = "year", timevar = "id", direction = "wide")
votediff <- votediff[with(votediff, order(year)), ] # sort

votediff <- na.interpolation(votediff, option = "linear") # impute missing feature values via linear interpolation 

# Labels

votediff.y <- data.frame(fg.ads.treat)
votediff.y <- votediff.y[with(votediff.y, order(year)), ] # sort

votediff.y.imp <- na.interpolation(votediff.y, option = "linear") # impute missing labels via linear interpolation

#votediff.y <- cbind(votediff.y['year'],"y.true"=rowMeans(votediff.y[-1], na.rm=TRUE)) # take treated mean

# Splits

votediff.years <- sort(intersect(votediff$year,votediff.y.imp$year)) # common years in treated and control

votediff.x.train <- votediff[votediff$year %in% votediff.years & votediff$year < 2005,]
votediff.x.test <- votediff[votediff$year %in% votediff.years & votediff$year >= 2005,]

votediff.y.train <- votediff.y.imp[votediff.y.imp$year %in% votediff.years & votediff.y.imp$year < 2005,]
votediff.y.test <- votediff.y.imp[votediff.y.imp$year %in% votediff.years & votediff.y.imp$year >= 2005,]

votediff.x.train <-  votediff.x.train[ , colSums(is.na(votediff.x.train)) == 0] # Remove training features with NA
votediff.x.test <-  votediff.x.test[ , colSums(is.na(votediff.x.test)) == 0] # Remove test features with NA

votediff.y.train <-  votediff.y.train[ , colSums(is.na(votediff.y.train)) == 0] # Remove training labels with NA
votediff.y.test <-  votediff.y.test[ , colSums(is.na(votediff.y.test)) == 0] # Remove test labels with NA

# Export each as csv (labels, features)

write.csv(votediff.x.train[!colnames(votediff.x.train) %in% c("year")], paste0(data.directory,"elections/treated/votediff-x-train.csv"), row.names=FALSE) 
write.csv(votediff.x.test[!colnames(votediff.x.test) %in% c("year")] , paste0(data.directory,"elections/treated/votediff-x-test.csv"), row.names=FALSE) 

write.csv(votediff.y.train[!colnames(votediff.y.train) %in% c("year")], paste0(data.directory,"elections/treated/votediff-y-train.csv"), row.names=FALSE) 
write.csv(votediff.y.test[!colnames(votediff.y.test) %in% c("year")], paste0(data.directory,"elections/treated/votediff-y-test.csv"), row.names=FALSE) 