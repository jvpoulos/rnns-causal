# Permutation test under null hypothesis 
# Intervention year is fixed

PermutationTest<-function(forecast, true, t.stat, n.placebo, np=NULL){ 
  # Calculate randomization test p-value.
  #
  # Args:
  #   forecast: post-period predictions
  #   true: post-period groundtruth
  #   t.stat: real treated t stat
  #   n.placebo: number of placebo treated units
  #   np: override n. possible placebo avgs. Default is NULL. 
  # Returns:
  #   Vector of per-time-step p values
  if(is.null(np)){
    np <- sum(choose(n.placebo,c(1:(n.placebo-1)))) # num. possible placebo avgs. 
  }
  placebo.mean <- matrix(NA, np, nrow(forecast) )
  p.count <- matrix(NA, np, nrow(forecast) )
  for(i in 1:np){
    treat <- sample(c(1:n.placebo),sample(c(1:(n.placebo-1)),1), replace=FALSE)
    
    placebo.impacts <- true[,treat]-forecast[,treat]
    if(!is.null(nrow(placebo.impacts))){
      placebo.mean[i,] <- as.numeric(rowMeans(placebo.impacts)) # Estimate a per-period placebo pooled intervention effect
    } else{
      placebo.mean[i,] <- as.numeric(placebo.impacts)
    }
    p.count[i,] <- as.numeric(ifelse(abs(placebo.mean[i,]) >=  abs(t.stat),1,0)) # 1 if fake t stat is more extreme than real
  }
  p.value <- colSums(p.count)/np # two-sided test
  return(p.value)
}

# Invert for CIs

PermutationCI <- function(forecast, true, t.stat, n.placebo, np=NULL, alpha=0.025, l=100, prec=1e-03) {
  require(matrixStats)
  # Calculate randomization test confidence interval.
  #
  # Args:
  #   alpha: Two-sided significance level. Default is 0.025.
  #   l: Number of constant treatment effects. Default is 100.
  #   np: override n. possible placebo avgs. Default is NULL. 
  #   prec: Level of precision in constant treatment effects. Default is 1e-05.
  #
  # Returns:
  #   Vector of per-time-step randomization confidence interval
  # Create vector to store CIs
  c.range <- round(range(t.stat),2)*2
  
  CI <- matrix(NA, nrow(forecast), l)
  for(i in 1:l){
    # Choose constant treatment effect
    delta.c <- sample(seq(c.range[1],c.range[2],by=prec),length(t.stat),replace=FALSE)
    # Subtract from t.stat
    t.stat.delta <- t.stat-delta.c
    # Run permuation test
    results <- PermutationTest(forecast, true, t.stat=t.stat.delta, n.placebo, np)
    # If result not significant, delta.c is in confidence interval
    CI[,i] <- ifelse(results>(2*alpha),delta.c,NA)
  }
  return(rowRanges(CI,na.rm=TRUE))
}