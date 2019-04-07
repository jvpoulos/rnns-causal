### Dan Hopkins
### 9/13/2016
### replication code: table 1

### load libraries
library(tm)
library(stm)
library(SnowballC)
library(lda)

#### user must download indfiles.zip and place them in a folder


#### identify the location of the text files
txt.papers <- "C:/users/danhop/Dropbox/cityelections/text/indfiles/"
(txt <- Corpus(DirSource(txt.papers,encoding="UTF-8"),readerControl = list(language = "eng",reader=readPlain)))

corpus.plat <- Corpus(VectorSource(txt))
n <- length(corpus.plat)
nn <- round(n*.02)

Sys.setlocale("LC_COLLATE", "C")

papers.dta <- DocumentTermMatrix(corpus.plat,
 control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
 removeNumbers = TRUE, removePunctuation = TRUE,minDocFreq=1))
dim(papers.dta)

#### check for non-empty papers
vec <- apply(papers.dta,1,sum)
vec
table(vec)

### all have more than 40?
sum(vec > 40)/length(vec)==1

papers.b.dta.1 <- papers.dta[! vec==0,]
dim(papers.b.dta.1)

### extract file names; remove rownames with empty papers
rn <- names(txt)
rn.sub <- rn[! vec==0]
length(rn.sub)==dim(papers.b.dta.1)[1]

######
######

library(lda)

#### remove stopwords
idx <- c(which(colnames(papers.b.dta.1) %in% stopwords("en")))
CT_dtm30A <- papers.b.dta.1[,-idx]

### identify share of documents containing each word
v1 <- apply(CT_dtm30A[,1:10000] > 0,2,sum)/dim(CT_dtm30A)[1]
v2 <- apply(CT_dtm30A[,10001:20000] > 0,2,sum)/dim(CT_dtm30A)[1]
v3 <- apply(CT_dtm30A[,20001:30000] > 0,2,sum)/dim(CT_dtm30A)[1]
v4 <- apply(CT_dtm30A[,30001:40000] > 0,2,sum)/dim(CT_dtm30A)[1]
v5 <- apply(CT_dtm30A[,40001:50000] > 0,2,sum)/dim(CT_dtm30A)[1]
v6 <- apply(CT_dtm30A[,50001:60000] > 0,2,sum)/dim(CT_dtm30A)[1]
v7 <- apply(CT_dtm30A[,60001:(dim(CT_dtm30A)[2])] > 0,2,sum)/dim(CT_dtm30A)[1]

vt <- c(v1,v2,v3,v4,v5,v6,v7)

### set threshold for word appearance
thres <- .025
CT_dtm30B <- CT_dtm30A[,vt > thres & vt < 1-thres]

vt2 <- vt[vt > thres & vt < 1-thres]
CT_dtm30D <- as.data.frame(as.matrix(CT_dtm30B))

####
### remove words

rm.words <- c("austin","americanstatesman","adler","texa","steve","michael",
"washington","lanier","gray","philadelphia","nutter","inquir","pope","kenney",
"pcom","franci","san","jose", "los" ,"angel","liccardo" ,"california", "mercuri",
 "garcetti" ,"sam", "santa",  "clara","silicon" , "bay","blasio" , "daili" ,
 "york","new","cuomo"  ,"copyright", "nypd"  , "brooklyn" , "bratton" ,
"washingtonpostcom" ,"mendelson" ,"muriel" ,"post","bowser" ,"washington" ,
"maryland" ,  "virginia", "hogan","yorker","fermino","jennif","documenttype",
"curri","campaign","florida" ,"brown","jacksonvill","timesunion","jacksonville",
"chicago","emanuel","john","paso","manhattan","albani","robert","william",
"thoma","juli", "david","oscar" )

CT_dtm30E2 <- CT_dtm30D[ ,! colnames(CT_dtm30D) %in% rm.words]


### check sizes
length(rn.sub)==dim(CT_dtm30D1)[1]

translate.matrix <- matrix(0,2,dim(CT_dtm30E2)[2])
translate.matrix[1,] <- 0:(dim(CT_dtm30E2)[2]-1)
#translate.matrix[2,1] <- 3
n <- dim(CT_dtm30E2)[1]

### first document
text.doc <- (CT_dtm30E2[1,])
which.words <- which(text.doc > 0)
ww <- length(which.words)
translate.matrix[2,1:ww] <- which.words
filled.to <- length(which.words)
for(i in 2:n){
	text.doc <- (CT_dtm30E2[i,])
	which.words <- which(text.doc > 0)
	idx.rem <-  ! which.words %in% translate.matrix[2,]
	which.words.new <- which.words[idx.rem]
	ww <- length(which.words.new)
	if(length(which.words.new) > 0){
		translate.matrix[2,(filled.to+1):(filled.to+ww)] <- which.words.new
		filled.to <- filled.to+ww
	}
	if(filled.to==dim(CT_dtm30E2)[2]){
		stop()
	}
}

#### convert data structure
doc <- list()
n <- dim(CT_dtm30E2)[1]
for(i in 1:n){
	text.doc <- (CT_dtm30E2[i,])
	which.words <- which(text.doc > 0)
	newvec <- c()
	for(j in 1:length(which.words)){
		newvec <- c(newvec,which(translate.matrix[2,]==which.words[j])-1)

	}
	text.doc.trunc <- unlist(text.doc)[which.words]
	doc[[i]] <- as.matrix(rbind(as.integer(newvec),as.integer(text.doc.trunc)))
}
order(translate.matrix[2,])

doc.vocab <- colnames(CT_dtm30E2)[translate.matrix[2,]]


#### try LDA
K <- 18
set.seed(192873)
result <- lda.collapsed.gibbs.sampler(documents=doc,
                                       K,  ## Num clusters
                                       doc.vocab,
                                       20000,  ## Num iterations
                                       alpha=0.1,
                                       eta=0.5,
                                       burnin=5000,
                                       compute.log.likelihood=TRUE) 

#save(doc,doc.vocab,result,file="C:/users/danhop/Dropbox/cityelections/text/lda-03112016C.Rdata")
#load("C:/users/danhop/Dropbox/cityelections/text/lda-03112016C.Rdata")

top.words <- top.topic.words(result$topics, 20, by.score=TRUE)

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
 
topic.prop.col <- apply(topic.proportions,2,mean)
sum(topic.prop.col^2)
# with alpha = 1
#[1] 0.06872535

rownames(topic.proportions) <- names(txt)
 
top.words.ed <- top.words[,-c(4,6,13,15)]

top.words
library(xtable)
xtable(top.words[,1:6])

topic.proportions.na <- na.omit(topic.proportions)
topic.proportions.ed <- topic.proportions[,-c(4,6,13,15)]

top.words2 <- top.words.ed[,order(apply(topic.proportions.ed,2,mean),decreasing=T)]
top.prop.2 <- apply(topic.proportions.ed,2,mean)[order(apply(topic.proportions.ed,2,mean),decreasing=T)]

library(xtable)
xtable(rbind(top.words2[1:10,1:7],round(top.prop.2[1:7],digits=3)))
xtable(rbind(top.words2[1:10,8:14],round(top.prop.2[8:14],digits=3)))

#xtable(rbind(top.words2[1:10,13:18],round(top.prop.2[13:18],digits=3)))
#xtable(top.words2[1:10,7:12])
#xtable(top.words2[1:10,13:18])


