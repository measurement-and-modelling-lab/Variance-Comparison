agostino1971<- function(x){
  ## D'Agostino, R.B. (1971), An omnibus test of normality for moderate and large size samples
  ## https://doi.org/10.1093/biomet/58.2.341
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat24.cpp
  
  n <- length(x)
  varX <- meanX <- Tx <- 0
  x<-sort(x)
  meanX <- mean(x)
  for(i in 1:n){
    varX <- varX + x[i]^2
  }
  varX <- varX/n - meanX^2
  for(i in 2:(n+1)){
    Tx <- Tx + ((i-1)-0.5*(n+1))*x[i-1]
  }
  D <- Tx/((n*n)*sqrt(varX))
  statDa <- sqrt(n)*(D - 0.28209479)/0.02998598
  
  result <- list(method = "D'Agostino (1971)",
                 statistic = statDa,
                 df = NA,
                 p.value = NA)
  return(result)
}
