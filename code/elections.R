###################################
# Assemble mayoral elections dataset   #
###################################

library(tidyr)
library(dplyr)
library(weights)
library(stringr)
library(foreign)
library(imputeTS)
library(caret)

## Load H&P (2017) data
hopkins <- read.table("data/hopkins/mayoralelections_final_full.tab",
                  header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)

# Extract state abbr

hopkins$state <- str_sub(gsub("\\d+|NA", "", hopkins$citystabbryear),start= -2)

# Clean city names

hopkins$city[hopkins$city=="Louisville-Jefferson County"] <- "Louisville"

# Subset relevant columns
hopkins <- hopkins[c("state","city","year","diffwinnerandloser")]

colnames(hopkins) <- c("state","city","year","votediff")

## Load F&G (2009) data

fg <- readstata13::read.dta13("data/ferreira/mayoral_data_4.17.09.dta")

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

# Make covariate dummies
fg$partisan2 <- NA
fg$partisan2[fg$partisan=="NP"] <- 0
fg$partisan2[fg$partisan=="P"] <- 1

fg$mayor_dem <- NA
fg$mayor_dem[fg$mayor_party=="D"] <- 1
fg$mayor_dem[fg$mayor_party=="R"] <- 0

fg$logvotetotal <- log(fg$vote_total) # make log vote total

# Take means for multiple elections in same year

fg <- fg %>% 
  filter(!is.na(votediff)) %>% 
  group_by(state, city, year) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

fg.covars <- fg[c('city','state','year',"partisan2","mayor_dem","logvotetotal")] # Save covariates

fg <- data.frame(fg[c("state", "city", "year","votediff")])

## Append mayoral elections

fg.hopkins <- rbind(fg, hopkins[paste(hopkins$city,hopkins$state,hopkins$year) %in%
                                  setdiff(paste(hopkins$city,hopkins$state,hopkins$year),
                                        paste(fg$city,fg$state,fg$year)),])

fg.hopkins <- fg.hopkins[with(fg.hopkins, order(state, city, year)), ] # sort

# Clean up

fg.hopkins <- fg.hopkins[!colnames(fg.hopkins) %in% c("votediff.y")]
colnames(fg.hopkins)[colnames(fg.hopkins)%in%"votediff.x"] <-"votediff"

fg.hopkins$city <- gsub("[^[:alnum:] ]", "",fg.hopkins$city) # clean name 
fg.hopkins$city <- gsub(" ", "",fg.hopkins$city)  

# Subset to cities with positive, non-missing vote difference

fg.hopkins <- fg.hopkins[!is.na(fg.hopkins$votediff) & fg.hopkins$votediff>0,]

fg.hopkins <- fg.hopkins[with(fg.hopkins, order(state, city, year)), ] # sort

fg.hopkins$id <- paste(fg.hopkins$city, fg.hopkins$state,sep=".")

# Reshape

votediff <- reshape(data.frame(fg.hopkins)[c("year","id","votediff")], idvar = "year", timevar = "id", direction = "wide")
votediff <- votediff[with(votediff, order(year)), ] # sort

rownames(votediff) <- votediff$year
votediff <- votediff[!colnames(votediff) %in% c("year")]

# Export 

elections.outcomes <- list("votediff"=votediff)

ElectionsMatrices <- function(d, outcomes=TRUE, imp=c("locf","linear","random","median")) {
  
  # Masked matrix for which 1=observed, NA=missing/imputed
  d.M.missing <- t(as.matrix(d))
  d.M.missing[is.nan(d.M.missing)] <- NA
  d.M.missing[!is.na(d.M.missing)] <-1
  
  # impute missing
  d.imp <- d
  if(outcomes){
    if(imp=="locf"){
      d.imp <- na.locf(d, option = "locf", na.remaining = "rev") 
    }
    if(imp=="linear"){
      d.imp <- na.interpolation(d, option = "linear")
    }
    if(imp=="random"){
      d.imp<- na.random(d)
    }
    if(imp=="median"){
      preProcValues <- preProcess(d, method = c("medianImpute"), verbose=TRUE) # use training set median
      d.imp <- predict(preProcValues, d)
    }
  } 
  
  d.imp <- log(d.imp+.Machine
               $double.eps) # take log
  
  # Matrix of observed entries (N x T)
  d.M <- t(as.matrix(d.imp))
  d.M[is.nan(d.M )] <- NA
  
  rownames(d.M) <- colnames(d)
  colnames(d.M) <- rownames(d)
  
  if(outcomes){
    
    # Masked matrix which is 0 for control units and treated units before treatment and 1 for treated units after treatment.
    
    d.mask <- matrix(0, nrow = nrow(d.M), 
                     ncol= ncol(d.M),
                     dimnames = list(rownames(d.M), colnames(d.M)))
    
    return(list("M"=d.M, "M.missing"=d.M.missing, "mask"=d.mask))
  } else{
    return(d.M)
  }
}

elections.outcomes.locf <- lapply(elections.outcomes, ElectionsMatrices, outcomes=TRUE, imp="locf")

saveRDS(elections.outcomes.locf, "data/elections-outcomes-locf.rds")