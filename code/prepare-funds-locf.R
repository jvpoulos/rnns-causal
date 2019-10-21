###################################
# Prepare education spending data for RNNs #
###################################

funds <- readRDS("data/capacity-outcomes-locf.rds")[['educ.pc']]

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

funds.covars <- readRDS("data/capacity-covariates.rds")

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
  
write.csv(train_data,"data/educ-x-locf.csv",row.names = FALSE)
write.csv(test_data,"data/educ-y-locf.csv",row.names = FALSE)
write.csv(train_w,"data/educ-wx-locf.csv",row.names = FALSE)
write.csv(test_w,"data/educ-wy-locf.csv",row.names = FALSE)

## Plot public education spending (pre) by treatment status

require(ggplot2)
require(reshape2)

df <- data.frame(t(Y)[1:(t0-1),][,colnames(t(Y))%in% c(colnames(train_data),colnames(test_data))],check.names = FALSE) #pre-period

df$id <- rownames(df)

df.m <- melt(df, "id")
df.m$status <- factor(ifelse(df.m$variable%in%treated.indices,"Treated","Control"))

df.m <- merge(df.m, t(rbind("weights"=predicted.all,"variable"=(names(predicted.all)))), by="variable", all.x = TRUE)

df.m$weights <- as.numeric(levels(df.m$weights))[df.m$weights]
df.m$weights[df.m$status=="Control"] <- df.m$weights[df.m$status=="Control"] /sum(df.m$weights[df.m$status=="Control"] ) # normalize weights per group
df.m$weights[df.m$status=="Treated"] <- df.m$weights[df.m$status=="Treated"] /sum(df.m$weights[df.m$status=="Treated"] ) # 

p <- ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill=status), alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Density") + 
  xlab("Log per-capita education spending") +
  scale_fill_manual(values = c("white","blue"), labels= c("Control", "Treated"), name="Treatment status") +
  theme(legend.position = "none"
        , axis.text=element_text(size=14)
        , axis.title.x=element_text(size = 16)
        , axis.title.y=element_text(size = 16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

ggsave("results/plots/educ-dens.png", p, width=8.5, height=11)

pw <- ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill=status, weights=weights), alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Density") + 
  xlab("Log per-capita education spending") +
  scale_fill_manual(values = c("white","blue"), labels= c("Control", "Treated"), name="Treatment status") +
  theme(legend.position = "none"
         , axis.text=element_text(size=14)
         , axis.title.x=element_text(size = 16)
         , axis.title.y=element_text(size = 16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

ggsave("results/plots/educ-dens-w.png", pw, width=8.5, height=11)

# two-sample t-test
X.c <- train_data[as.numeric(rownames(train_data))<1869,]
X.t <-test_data[as.numeric(rownames(train_data))<1869,]

t.test(X.t,X.c,alternative="two.sided", conf.level = 0.95)

library(weights)

wtd.t.test(X.c, X.t, weight=train_w[1:(t0-1),], weighty=test_w[1:(t0-1),], samedata = FALSE, bootse=TRUE) 