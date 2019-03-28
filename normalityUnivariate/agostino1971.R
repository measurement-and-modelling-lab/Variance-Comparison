agostino1971<- function(x){
  ## D'Agostino, R.B. (1971), An omnibus test of normality for moderate and large size samples
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat24.cpp
  
  n <- length(x)
  x<-sort(x)
  meanX <- mean(x)
  varX <- sum(x^2)
  varX <- varX/n - meanX^2
  Tx <- sum((1:n - 0.5 * (n + 1)) * x)
  D <- Tx/((n*n)*sqrt(varX))
  statDa <- sqrt(n)*(D - 0.28209479)/0.02998598
  
  result <- list(method = "D'Agostino (1971)",
                 doi = "https://doi.org/10.1093/biomet/58.2.341",
                 statistic = statDa,
                 df = NA,
                 p.value = NA)
  return(result)
}
