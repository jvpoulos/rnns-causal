###################################
# Prepare education spending data for RNNs #
###################################

funds <- readRDS("/media/jason/Dropbox/github/land-reform/data/capacity-outcomes.rds")[['educ.pc']]

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

# Converting the data to a floating point matrix
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

predicted <- plogis(predict(logitMod, X[-training.indices,][,-ncol(X)]))  # predicted scores

library(InformationValue)
plotROC(X[-training.indices,][,ncol(X)], predicted) # ROC 83.33%

predicted.all <- plogis(predict(logitMod, X))  # predicted scores on full training set
predicted.train <- predicted.all[names(predicted.all) %in% colnames(train_data)]

predicted.train <-predicted.train[order(match(names(predicted.train),colnames(train_data)))]

train_w <- matrix(predicted.train ,nrow=nrow(train_data),ncol=length(predicted.train),byrow=TRUE)

predicted.test <- predicted.all[names(predicted.all) %in% colnames(test_data)]

predicted.test <-predicted.test[order(match(names(predicted.test),colnames(test_data)))]

test_w <- matrix(predicted.test ,nrow=nrow(test_data),ncol=length(predicted.test),byrow=TRUE)

write.csv(train_data,paste0(data.directory,"educ-x.csv"),row.names = FALSE)
write.csv(test_data,paste0(data.directory,"educ-y.csv"),row.names = FALSE)
write.csv(train_w,paste0(data.directory,"educ-wx.csv"),row.names = FALSE)
write.csv(test_w,paste0(data.directory,"educ-wy.csv"),row.names = FALSE)

## Plot public education spending (pre) by treatment status

require(ggplot2)
require(reshape2)

df <- data.frame(t(Y)[1:(t0-1),][,colnames(t(Y))%in% c(colnames(train_data),colnames(test_data))],check.names = FALSE) #pre-period

df$id <- rownames(df)

df.m <- melt(df, "id")
df.m$status <- factor(ifelse(df.m$variable%in%treated.indices,"Treated","Control"))

df.m <- merge(df.m, t(rbind("weights"=predicted.all,"variable"=(names(predicted.all)))), by="variable", all.x = TRUE)

df.m$weights <- as.numeric(levels(df.m$weights))[df.m$weights]

p <- ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill=status), alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Density") + 
  xlab("Log per-capita education spending") +
  scale_fill_manual(values = c("red","blue"), labels= c("Control", "Treated"), name="Treatment status") +
  theme(legend.justification = c(0.95, 0.95), legend.position = c(0.25, 0.95),legend.background = element_rect(colour = "black"))

ggsave(paste0(results.directory,"plots/educ-dens.png"), p, width=8.5, height=11)

pw <- ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill=status,weights=weights), alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Density") + 
  xlab("Log per-capita education spending") +
  scale_fill_manual(values = c("red","blue"), labels= c("Control", "Treated"), name="Treatment status") +
  theme(legend.justification = c(0.95, 0.95), legend.position = c(0.25, 0.95),legend.background = element_rect(colour = "black"))

ggsave(paste0(results.directory,"plots/educ-dens-w.png"), pw, width=8.5, height=11)

# two-sample t-test
X.c <- train_data[as.numeric(rownames(train_data))<1869,]
X.t <-test_data[as.numeric(rownames(train_data))<1869,]

t.test(X.t,X.c,alternative="two.sided", conf.level = 0.95)

library(weights)

wtd.t.test(X.c, X.t, weight=train_w[1:(t0-1),], weighty=test_w[1:(t0-1),], samedata = FALSE, bootse=TRUE) 
