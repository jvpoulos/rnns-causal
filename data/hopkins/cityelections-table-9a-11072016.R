#### Dan Hopkins
#### Code to estimate Appendix Table 9A
#### 11/7/2016
#### "Retrospective Voting in Big-City U.S. Mayoral Elections"

library(texreg)

dta <- read.csv("/users/danhop/Dropbox/cityelections/datasets/most_recent/mayoralelections_final_full.csv")

dta10 <- dta[dta$cityinds==1 & ! dta$cityinds %in% c(NA),]

lout1 <- lm(incumbentpercent ~ DIFFusmonthcitymonth+ unemploymonthlyus,data=dta10)

lout2 <- lm(incumbentpercent ~ DIFFusmonthcitymonth+ unemploymonthlyus + logCENSUS2000pop+as.factor(year),data=dta10)

lout3 <- lm(incumbentpercent ~ DIFFusmonthcitymonth+ unemploymonthlyus + logCENSUS2000pop + CENSUS2000hispanicpercent + CENSUS2000blackpercent +CENSUS2000bachdegplus + CENSUS2000medhomevalue + CENSUS1990medhhinc + mayorcouncildum  + demvs1988+as.factor(year),data=dta10)
 
 texreg(l=list(lout1,lout2,lout3),stars=c(0.05),digits=3)