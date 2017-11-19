###################################
# Lasso estimates                       #
# Code from http://multithreaded.stitchfix.com/blog/2016/04/21/forget-arima/ 
###################################

library(e1071)

# Many lambdas for prediction errors

lasso.val <- glmnet::glmnet(x=as.matrix(votediff.x.train[-1][train.indices,]), 
                                    y=votediff.y.train$y.true[train.indices], 
                                    family = "gaussian", 
                                    alpha=1, 
                                    lambda=seq(25, 0.005, by=-0.001),
                                    intercept=TRUE,
                                    standardize=FALSE) 

lasso.test <- glmnet::glmnet(x=as.matrix(votediff.x.train[-1]), 
                            y=votediff.y.train$y.true, 
                            family = "gaussian", 
                            alpha=1, 
                            lambda=seq(25, 0.005, by=-0.001),
                            intercept=TRUE,
                            standardize=FALSE) 

lasso.pred.train <- glmnet::predict.glmnet(lasso.val, newx = as.matrix(votediff.x.train[-1][train.indices,]), # get training fit
                                         type = "response",
                                         s=lasso.val$lambda)

lasso.pred.val <- glmnet::predict.glmnet(lasso.val, newx = as.matrix(votediff.x.train[-1][val.indices,]), 
                                  type = "response",
                                  s=lasso.val$lambda)

lasso.pred.test <- glmnet::predict.glmnet(lasso.test, newx = as.matrix(votediff.x.test[-1]), 
                           type = "response",
                           s=lasso.test$lambda)

lasso.pred.train.y <- rowMeans(lasso.pred.train[,-1] )
lasso.pred.val.y <- rowMeans(lasso.pred.val[,-1] )
lasso.pred.test.y <- rowMeans(lasso.pred.test[,-1] )

lasso.pred.val.sd <- matrixStats::rowSds(lasso.pred.val[,-1] )
lasso.pred.test.sd <- matrixStats::rowSds(lasso.pred.test[,-1] )

# Actual versus predicted
lasso.d2 <- data.frame(
  votediff.y,  # actual data and dates 
  c(lasso.pred.train.y, lasso.pred.val.y, lasso.pred.test.y), # predictions
  c(rep(NA, 42), lasso.pred.val.sd, lasso.pred.test.sd))  
 
names(lasso.d2) <- c("Date", "Actual", "Fitted", "SD")

# MAPE (mean absolute percentage error) on validation set
lasso.MAPE <- filter(lasso.d2, Date %in% c(2000:2004)) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
lasso.MAPE*100

# Plot actual versus predicted with prediction intervals for the holdout period
lasso.plot <- ggplot(data=lasso.d2, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=2000, linetype=3) + 
  geom_vline(xintercept=2005, linetype=2) + 
  geom_ribbon(aes(ymin=Fitted-(SD*1.96), ymax=Fitted+(SD*1.96)), fill="grey", alpha=0.5) +
  ggtitle(paste0("Lasso model (validation MAPE = ", round(100*lasso.MAPE,2), "%)")) +
  theme.blank

ggsave(paste0(results.directory,"plots/lasso-plot.png"), lasso.plot, width=11, height=8.5)

## Calculate pointwise impacts

lasso.d2$pointwise <- lasso.d2$Actual-lasso.d2$Fitted
lasso.d2$pointwise.lower <- lasso.d2$Actual-(lasso.d2$Fitted+abs(lasso.d2$SD)*1.96)
lasso.d2$pointwise.upper <- lasso.d2$Actual-(lasso.d2$Fitted-abs(lasso.d2$SD)*1.96)
