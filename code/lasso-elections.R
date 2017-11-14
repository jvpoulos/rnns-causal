###################################
# Lasso estimates                       #
# Code from http://multithreaded.stitchfix.com/blog/2016/04/21/forget-arima/ 
###################################

library(e1071)

# # Lasso with cv lambda
# 
# lasso.cv <- glmnet::cv.glmnet(x=as.matrix(votediff.x.train[-1][train.indices,]), 
#                               y=votediff.y.train$y.true[train.indices], 
#                               family = "gaussian", 
#                               type.measure = "mae",
#                               alpha=0.7, 
#                               parallel=TRUE,
#                               intercept=FALSE,
#                               standardize=FALSE) # already standardized
# 
# #coef(lasso.cv, s = "lambda.min")
# 
# lasso.cv$lambda.min
# 
# lasso.cv.pred.train <- predict.cv.glmnet(lasso.cv, newx = as.matrix(votediff.x.train[-1][train.indices,]), type = "response", s = "lambda.min")
# 
# lasso.cv.pred.test <- predict.cv.glmnet(lasso.cv, newx = as.matrix(rbind(votediff.x.train[-1][val.indices,], votediff.x.test[-1])),  type = "response", s = "lambda.min")

# Many lambdas for prediction errors
lasso <- glmnet(x=as.matrix(votediff.x.train[-1][train.indices,]), 
                                    y=votediff.y.train$y.true[train.indices], 
                                    family = "gaussian", 
                                    alpha=0.7, 
                                    nlambda=1000,
                                    intercept=FALSE,
                                    standardize=FALSE) # already standardized

lasso.pred.train <- predict.glmnet(lasso, newx = as.matrix(votediff.x.train[-1][train.indices,]), 
                                  type = "response",
                                  c=lasso$lambda)

lasso.pred.test <- predict.glmnet(lasso, newx = as.matrix(rbind(votediff.x.train[-1][val.indices,], votediff.x.test[-1])), 
                           type = "response",
                           c=lasso$lambda)

lasso.pred.train.y <- rowMeans(lasso.pred.train[,-1] )
lasso.pred.test.y <- rowMeans(lasso.pred.test[,-1] )

lasso.pred.test.sd <- matrixStats::rowSds(lasso.pred.test[,-1] )

# Actual versus predicted
lasso.d2 <- data.frame(
  votediff.y,  # actual data and dates 
  c(lasso.pred.train.y, lasso.pred.test.y), # fitted values and predictions
  c(rep(NA, length(lasso.pred.train.y)), lasso.pred.test.sd))  
 
names(lasso.d2) <- c("Date", "Actual", "Fitted", "SD")

# MAPE (mean absolute percentage error) on validation set
lasso.MAPE <- filter(lasso.d2, Date %in% c(2002,2003,2004)) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
lasso.MAPE*100

# Plot actual versus predicted with prediction intervals for the holdout period
lasso.plot <- ggplot(data=lasso.d2, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=2005, linetype=2) +
  geom_ribbon(aes(ymin=Fitted-SD, ymax=Fitted+SD), fill="grey", alpha=0.5) +
  ggtitle(paste0("Lasso model: Validation MAPE = ", round(100*lasso.MAPE,2), "%")) +
  theme.blank

ggsave(paste0(results.directory,"plots/lasso-plot.png"), lasso.plot, width=11, height=8.5)

## Calculate pointwise impacts

lasso.d2$pointwise <- lasso.d2$Actual-lasso.d2$Fitted
lasso.d2$pointwise.lower <- lasso.d2$Actual-(lasso.d2$Fitted+abs(lasso.d2$SD))
lasso.d2$pointwise.upper <- lasso.d2$Actual-(lasso.d2$Fitted-abs(lasso.d2$SD))
