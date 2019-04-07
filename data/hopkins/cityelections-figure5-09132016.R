### Dan Hopkins
### 9.13.2016

### Figure 5 replication file

#for final model: cityinds==1
#to re-add landslides: nolim==1 

library(foreign)

dta <- read.csv("/Users/jason/Dropbox/github/rnns-causal/data/hopkins/mayoralelections_final_full.csv")
#dta <- read.csv("/users/danhop/Dropbox/cityelections/datasets/most_recent/mayoralelections_final_full.csv")

vars <- read.csv("/users/danhop/Dropbox/cityelections/datasets/potentially_omitted_variablesC.csv",sep=c(",",";"))
nams <- read.csv("/users/danhop/Dropbox/cityelections/datasets/omittednames.csv",sep=c(",",";"))

vars2 <- cbind(vars,nams)
var.names.2 <- as.character(vars2[,1])
var.names.2B <- as.character(vars2[,2])

dta.final <- dta[dta$cityinds==1 & ! dta$cityinds %in% c(NA),]

rmat <- as.data.frame(matrix(NA,length(var.names.2),4))
for(i in 1:length(var.names.2)){
	txt <- paste("lout <- lm(incumbentpercent ~ DIFFusmonthcitymonth + ",var.names.2[i],"+ unemploymonthlyus + logCENSUS2000pop + CENSUS2000hispanicpercent + CENSUS2000blackpercent +CENSUS2000bachdegplus + CENSUS2000medhomevalue + CENSUS1990medhhinc + mayorcouncildum  + demvs1988+as.factor(year),data=dta.final)",sep="")
	eval(parse(text=txt))
	rmat[i,1:2] <- summary(lout)$coef[2,1:2]
	rmat[i,3:4] <- summary(lout)$coef[3,1:2]
}

rmat2 <- rmat[order(rmat[,1]),]
var.names.ord <- var.names.2B[order(rmat[,1])]

#pdf("/users/danhop/Dropbox/cityelections/paper/newest_update/omittedvars03082016A.pdf")
plot(x=rmat2[,1],y=1:dim(rmat2)[1],pch=16,ylim=c(0,length(var.names.2)+1),xlim=c(-.01,.1),yaxt="n",ylab="",
main="Robustness to Including \n Other Variables",cex.main=1.6,xlab="Coefficient")
abline(v=0,lty=2)

for(i in 1:length(var.names.ord)){
	lines(y=c(i,i),x=c(rmat2[i,1]-1.96*rmat2[i,2],rmat2[i,1]+1.96*rmat2[i,2]))
	text(var.names.ord[i],x=0.075,y=i)
}
#dev.off()



