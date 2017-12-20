Run2Stage <- function(data, indices, f1, f2, first.test=FALSE) {
  # Helper function for 2-stage GBR regression
  d <- data[indices,]
  if(first.test==FALSE){
    # Second model
    second <- lm(f2,d, treat==0) 
    d$delta <- as.vector(unlist(d[paste(f2[[2]])])) - (second$coefficients[[1]] + second$coefficients[[2]]*as.vector(unlist(d[paste(f2[[3]])])))
    d$delta[d$treat==0] <- 0
  }else{ # no spending in j-1
    d$delta <- as.vector(unlist(d[paste(f2[[2]])]))
  }
  # First model
  first <- lm(f1, d)
  return(coef(first))
}