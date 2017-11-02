Run2Stage <- function(data, indices) {
  d <- data[indices,]
  fit <- lm(post ~ pre + grp_buy + factor(strata), weights=1/post, d)
  coef(fit)
}