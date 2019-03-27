spiegelhalter1977 <- function(x){
  ## A test for normality against symmetric alternatives
  ## https://doi.org/10.1093/biomet/64.2.415
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat41.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
  n <- length(x)
  varX  <- 0
  M_PI <- 3.14159265358979323846
  M_E <- 2.71828182845904523536
  max <- max(x)
  min <- min(x)
  meanX <- mean(x)
 
  for (i in 1:n) {
    varX <- varX + (x[i]-meanX)^2
  }
  varX <- varX/(n-1)
  sd <- sqrt(varX)
  u <- (max-min)/sd
  g <- 0
  for (i in 1:n) {
    g<- g + abs(x[i]-meanX)
  }
  g <- g/(sd*sqrt(n)*sqrt(n-1))
  if (n < 150) {
    cn <- 0.5*(gamma((n+1)))^(1.0/(n-1))/n
  } else {
    cn <- (2*M_PI)^(1/(2*(n-1)))*((n*sqrt(n))/M_E)^(1/(n-1))/(2*M_E)
    
  }  
  statSp <- (cn*u^(-(n-1)) + g^(-(n-1)))^(1.0/(n-1))
  
          
  result <- list(method = "Spiegelhalter, D.J. (1977)",
                 statistic = statSp,
                 df = NA,
                 p.value = NA)
  
  return(result)
  
  
  
}