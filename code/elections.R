###################################
# Assemble city elections dataset   #
###################################

library(tidyr)
library(dplyr)

data.directory <- "~/Dropbox/github/rnns-causal/data/"

## Load H&P (2017) data
hopkins <- read.table(paste0(data.directory,"hopkins/mayoralelections_final_full.tab"),
                  header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)

# Extract state abbr

hopkins$state <- str_sub(gsub("\\d+|NA", "", hopkins$citystabbryear),start= -2)

# Clean city names

hopkins$city[hopkins$city=="Louisville-Jefferson County"] <- "Louisville"

# Subset relevant columns
hopkins <- hopkins[c("state","city","year","diffwinnerandloser","incumbentpercent")]

colnames(hopkins) <- c("state","city","year","votediff","incumbentshare")

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

fg <- fg[c("state","city","year","votediff","incumbentshare")]

# Take means for multiple elections in same year

fg <- fg %>% 
  filter(!is.na(votediff) | !is.na(incumbentshare)) %>% 
  group_by(state, city, year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

fg <- data.frame(fg)

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

ads <- ads[c("state","city","year","votesharediff","year_exp", "grp_buy","contested_2006", "inc_running_2006")]

colnames(ads) <- c("state","city","year","votediff","year_exp", "grp_buy","contested_2006", "inc_running_2006")

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

fg.ads <- fg.ads  %>% group_by(city, state) %>% fill(treat, year_exp, grp_buy, contested_2006, inc_running_2006, .direction="down") # fill missing
fg.ads <- fg.ads  %>% group_by(city, state) %>% fill(treat,year_exp, grp_buy, contested_2006, inc_running_2006, .direction="up") # fill missing

# Clean up
fg.ads <- fg.ads[c("state","city","year","votediff.x","incumbentshare","year_exp","grp_buy","contested_2006", "inc_running_2006", "treat")]
colnames(fg.ads) <- c("state","city","year","votediff","incumbentshare","year_exp","grp_buy","contested_2006", "inc_running_2006", "treat")

# Remove if missing both vote share measures

fg.ads <- fg.ads[!is.na(fg.ads$votediff) | !is.na(fg.ads$incumbentshare),]
