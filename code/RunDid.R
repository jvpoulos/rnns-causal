RunDiD <- function(my.data, indices){
  d <- my.data[indices,]
  return(
    (mean(d$y[d$time==1 & d$treat==1]) -
       mean(d$y[d$time==1 & d$treat==0])) -
      (mean(d$y[d$time==0 & d$treat==1]) -
         mean(d$y[d$time==0 & d$treat==0]))
  )
}