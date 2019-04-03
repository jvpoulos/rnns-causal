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

Y_obs <- Y * treat_mat

# Converting the data to a floating point matrix
data <- data.matrix(t(Y_obs)) # T x N

# Splits
train_data <- data[,(-treat_indices)] # train on control units

test_data <- data[,(treat_indices)] # treated units

write.csv(train_data,paste0("../data/",d,"-x.csv"),row.names = FALSE)
write.csv(test_data,paste0("../data/",d,"-y.csv"),row.names = FALSE)