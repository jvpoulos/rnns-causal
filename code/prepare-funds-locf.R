###################################
# Prepare education spending data for RNNs #
###################################

library(caret)
library(imputeTS)
library(glmnet)

# Read data
capacity.outcomes <- readRDS("data/capacity-outcomes-locf.rds")
capacity.covariates <- readRDS("data/capacity-covariates.rds")

# Transform covars to unit and time-specific inputs
capacity.covars.x <- as.matrix(bind_rows(lapply(capacity.covariates,rowMeans))) # N x # predictors
rownames(capacity.covars.x) <- rownames(capacity.covariates$faval)

# matrix of same time dimension as outcomes
capacity.covars.z <- list("faval"=matrix(NA, nrow = nrow(capacity.outcomes$educ.pc$M), ncol = ncol(capacity.outcomes$educ.pc$M)),
                          "farmsize"=matrix(NA, nrow = nrow(capacity.outcomes$educ.pc$M), ncol = ncol(capacity.outcomes$educ.pc$M)),
                          "access"=matrix(NA, nrow = nrow(capacity.outcomes$educ.pc$M), ncol = ncol(capacity.outcomes$educ.pc$M)))

for(i in c("faval","farmsize","access")){
  colnames(capacity.covars.z[[i]]) <- colnames(capacity.outcomes$educ.pc$M)
  rownames(capacity.covars.z[[i]]) <- rownames(capacity.outcomes$educ.pc$M)
}

colnames(capacity.covariates$faval) <- sub('faval.', '', colnames(capacity.covariates$faval))
colnames(capacity.covariates$farmsize) <- sub('farmsize.', '', colnames(capacity.covariates$farmsize))
colnames(capacity.covariates$access) <- sub('track2.', '', colnames(capacity.covariates$access))

# fill in observed data and impute missing 

for(i in c("faval","farmsize","access")){
  for(n in rownames(capacity.outcomes$educ.pc$M)){
    for(t in colnames(capacity.outcomes$educ.pc$M)){
      capacity.covars.z[[i]][n,][t] <- capacity.covariates[[i]][n,][t]
    }
  }
  
  capacity.covars.z[[i]] <- t(capacity.covars.z[[i]])
  preProcValues <- preProcess(capacity.covars.z[[i]], method = c("medianImpute"), verbose=TRUE) # use training set median
  
  capacity.covars.z[[i]] <- na_locf(capacity.covars.z[[i]], option = "locf", na_remaining = "keep") 
  
  capacity.covars.z[[i]] <- predict(preProcValues, capacity.covars.z[[i]])
  
  capacity.covars.z[[i]] <- t(capacity.covars.z[[i]])
}

capacity.covars.z <- as.matrix(bind_rows(lapply(capacity.covars.z,colMeans))) # T x # predictors
rownames(capacity.covars.z) <- colnames(capacity.outcomes$educ.pc$M)

# Prepare outcomes data
educ <- capacity.outcomes$educ.pc
Y.missing <- educ$M.missing # NxT
Y <- educ$M # NxT 

treat <- educ$mask # NxT masked matrix 

N <- nrow(treat)
T <- ncol(treat)

treated.indices <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
t0 <- which(colnames(Y)=="1869") # first treatment time # same for all outcomes

treat[rownames(treat)%in% treated.indices,][,as.numeric(colnames(treat)) >= 1869] <- 1# adjust for simultaneous adoption 

# Censor post-period treated values

treat_mat <- 1-treat
Y_obs <- Y * treat_mat
Y_imp <- Y * Y.missing

# converting the data to a floating point matrix
data <- data.matrix(t(Y_obs)) # T x N

# # Splits
train_data <- data[,!colnames(data)%in% c(treated.indices,"TN")] # train on control units

test_data <- data[,colnames(data)%in% treated.indices] # treated units

## Estimate propensity scores

capacity.covars.x <- capacity.covars.x[!rownames(capacity.covars.x)%in%c("GA"),] # GA not in outcomes, TN for parity
capacity.covars.z <- capacity.covars.z

logitMod.x <- cv.glmnet(x=capacity.covars.x, y=as.factor((1-treat_mat)[,t0]), family="binomial", nfolds= nrow(capacity.covars.x), parallel = TRUE, nlambda=400) # LOO

logitMod.z <- cv.glmnet(x=capacity.covars.z, y=as.factor((1-treat_mat)[treated.indices[1],]), family="binomial", nfolds=nrow(capacity.covars.z), parallel = TRUE, nlambda=400)

p.weights.x <- as.vector(predict(logitMod.x, capacity.covars.x, type="response", s ="lambda.min"))
p.weights.z <- as.vector(predict(logitMod.z, capacity.covars.z, type="response", s ="lambda.min"))

p.weights <- outer(p.weights.x,p.weights.z)   # outer product of fitted values on response scale
p.weights <- t(p.weights) # T x N
rownames(p.weights) <-rownames(data)
colnames(p.weights) <-colnames(data)

train_w <- p.weights[,colnames(p.weights)%in%colnames(train_data)] 
test_w <- p.weights[,colnames(p.weights)%in%colnames(test_data)] 
  
write.csv(train_data,"data/educ-x-locf.csv",row.names = FALSE)
write.csv(test_data,"data/educ-y-locf.csv",row.names = FALSE)
write.csv(train_w,"data/educ-wx-locf.csv",row.names = FALSE)
write.csv(test_w,"data/educ-wy-locf.csv",row.names = FALSE)

require(ggplot2)
require(reshape2)

df <- data.frame(t(Y)[1:(t0-1),][,colnames(t(Y))%in% c(colnames(train_data),colnames(test_data))],check.names = FALSE) #pre-period

df$id <- rownames(df)

df.m <- melt(df, "id")
df.m$status <- factor(ifelse(df.m$variable%in%treated.indices,"Treated","Control"))

df.weights <- data.frame(p.weights[1:(t0-1),][,colnames(p.weights)%in% c(colnames(train_w),colnames(test_w))],check.names = FALSE) #pre-period
df.weights$id <- rownames(df.weights)

df.weights.m <- melt(df.weights, "id")
df.weights.m$status <- factor(ifelse(df.weights.m$variable%in%treated.indices,"Treated","Control"))
colnames(df.weights.m) <- c("id","variable","weights","status" )

df.m <- cbind(df.m, df.weights.m[c("weights")])

# df.m$weights <- as.numeric(levels(df.m$weights))[df.m$weights]
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