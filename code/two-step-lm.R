library(e1071)

# Lasso with cv lambda


lasso.cv.val <- glmnet::cv.glmnet(x=as.matrix(votediff.x.train[-1][train.indices,]),
                                   y=votediff.y.train$y.true[train.indices],
                                   family = "gaussian",
                                   type.measure = "mae",
                                   alpha=1,
                                   parallel=TRUE,
                                   intercept=TRUE,
                                   standardize=FALSE)

lasso.cv.test <- glmnet::cv.glmnet(x=as.matrix(votediff.x.train[-1]),
                                    y=votediff.y.train$y.true,
                                    family = "gaussian",
                                    type.measure = "mae",
                                    alpha=1,
                                    parallel=TRUE,
                                    intercept=TRUE,
                                    standardize=FALSE)


## Two-step LM
lasso.vars.val <- names(coef(lasso.cv.val, s = "lambda.min")[,1][coef(lasso.cv.val, s = "lambda.min")[,1]!=0]) # non-zeroed out coefficients

lasso.vars.test <- names(coef(lasso.cv.test, s = "lambda.min")[,1][coef(lasso.cv.test, s = "lambda.min")[,1]!=0]) # non-zeroed out coefficients

lm2.val <- lm(y.true ~ ., data=cbind("y.true"=votediff.y.train$y.true[train.indices], votediff.x.train[train.indices,][colnames(votediff.x.train) %in% lasso.vars.val]))

lm2.test <- lm(y.true ~ ., data=cbind("y.true"=votediff.y.train$y.true, votediff.x.train[colnames(votediff.x.train) %in% lasso.vars.test]))

lm2.pred.val <- predict.lm(lm2.val, 
                       se.fit = TRUE,
                       newdata = votediff.x.train[val.indices,][colnames(votediff.x.train) %in% lasso.vars.val])

lm2.pred.test <- predict.lm(lm2.test, 
                           se.fit = TRUE,
                           newdata = votediff.x.test[colnames(votediff.x.test) %in% lasso.vars.test])
                             
# Actual versus predicted
lm2.d2 <- data.frame(
  votediff.y,  # actual data and dates 
  c(lm2.val$fitted.values, lm2.pred.val$fit, lm2.pred.test$fit), # fitted values and predictions
  c(rep(NA, length(lm2.val$fitted.values)), lm2.pred.val$se.fit, lm2.pred.test$se.fit))  # se fit

names(lm2.d2) <- c("Date", "Actual", "Fitted", "Fit.SE")

# MAPE (mean absolute percentage error) on validation set
lm2.MAPE <- filter(lm2.d2, Date %in% c(2000:2004)) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
lm2.MAPE*100

# Plot actual versus predicted with prediction intervals for the holdout period
lm2.plot <- ggplot(data=lm2.d2, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Observed"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Predicted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=2000, linetype=3) + 
  geom_vline(xintercept=2005, linetype=2) + 
  geom_ribbon(aes(ymin=Fitted-(Fit.SE*1.96), ymax=Fitted+(Fit.SE*1.96)), fill="grey", alpha=0.5) +
  ggtitle(paste0("Two-stage linear model (validation MAPE = ", round(100*lm2.MAPE,2), "%)")) +
  theme.blank

ggsave(paste0(results.directory,"plots/lm2-plot.png"), lm2.plot, width=11, height=8.5)

## Calculate pointwise impacts

lm2.d2$pointwise <- lm2.d2$Actual-lm2.d2$Fitted
lm2.d2$pointwise.lower <- lm2.d2$Actual-(lm2.d2$Fitted+abs(lm2.d2$Fit.SE)*1.96)
lm2.d2$pointwise.upper <- lm2.d2$Actual-(lm2.d2$Fitted-abs(lm2.d2$Fit.SE)*1.96)
