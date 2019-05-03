###################################
# Prepare education spending data for RNNs #
###################################

funds <- readRDS("/media/jason/Dropbox/github/land-reform/data/capacity-outcomes-median.rds")[['educ.pc']]

Y <- funds$M # NxT 

treat <- funds$mask # NxT masked matrix 

N <- nrow(treat)
T <- ncol(treat)

treated.indices <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
t0 <- which(colnames(Y)=="1869") # first treatment time # same for all outcomes

treat[rownames(treat)%in% treated.indices,][,as.numeric(colnames(treat)) >= 1869] <- 1# adjust for simultaneous adoption 

# Censor post-period treated values

treat_mat <- 1-treat
Y_obs <- Y * treat_mat

# converting the data to a floating point matrix
data <- data.matrix(t(Y_obs)) # T x N

# Splits
train_data <- data[,!colnames(data)%in% treated.indices] # train on control units

test_data <- data[,colnames(data)%in% treated.indices] # treated units
set.seed(1280)
train_data <- train_data[, sample(1:ncol(train_data), ncol(test_data))] # dimensional parity with test set

# importance weight matrix

funds.covars <- readRDS("/media/jason/Dropbox/github/land-reform/data/capacity-covariates.rds")

X <- data.frame(funds.covars) 
X$treat <- ifelse(rownames(X) %in% treated.indices, 1, 0)

set.seed(1280)
training.indices <- sample(nrow(X), ceiling(nrow(X)*0.75))

logitMod <- glm(treat ~ ., data=X[training.indices,], family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, X[-training.indices,][,-ncol(X)]))  # convert it into prediction probability scores that is bound between 0 and 1

library(InformationValue)
plotROC(X[-training.indices,][,ncol(X)], predicted) 

predicted.all <- plogis(predict(logitMod, X))  # predicted scores on full training set
predicted.train <- predicted.all[names(predicted.all) %in% colnames(train_data)]

predicted.train <-predicted.train[order(match(names(predicted.train),colnames(train_data)))]

train_w <- matrix(predicted.train ,nrow=nrow(train_data),ncol=length(predicted.train),byrow=TRUE)
rownames(train_w) <- rownames(train_data)
colnames(train_w) <- colnames(train_data)

# propensity score increases as t increase (penalize earlier weights)
train_w <- train_w*(log(1:nrow(train_data))-min(log(1:nrow(train_data))))/(max(log(1:nrow(train_data)))-min(log(1:nrow(train_data)))) # norm log values

predicted.test <- predicted.all[names(predicted.all) %in% colnames(test_data)]

predicted.test <-predicted.test[order(match(names(predicted.test),colnames(test_data)))]

test_w <- matrix(predicted.test ,nrow=nrow(test_data),ncol=length(predicted.test),byrow=TRUE)
rownames(test_w) <- rownames(test_data)
colnames(test_w) <- colnames(test_data)

# propensity score increases as t increase (penalize earlier weights)
test_w <- test_w*(log(1:nrow(test_data))-min(log(1:nrow(test_data))))/(max(log(1:nrow(test_data)))-min(log(1:nrow(test_data)))) # norm log values
  
write.csv(train_data,paste0(data.directory,"educ-x-median.csv"),row.names = FALSE)
write.csv(test_data,paste0(data.directory,"educ-y-median.csv"),row.names = FALSE)
write.csv(train_w,paste0(data.directory,"educ-wx-median.csv"),row.names = FALSE)
write.csv(test_w,paste0(data.directory,"educ-wy-median.csv"),row.names = FALSE)