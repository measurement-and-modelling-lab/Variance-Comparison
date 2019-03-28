cabana1994a <- function(x){
  ## The 1st Cabana-Cabana(1994) test for normality
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat19.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
  n <- length(x)
  varX <- meanX <- sdX <- meanH3 <- meanH4 <- meanH5 <- meanH6 <- meanH7 <- meanH8 <- 0
  zdata <- matrix(0,n,0)
  vectoraux1 <- matrix(0,n,0)
  meanX <- mean(x)
  varX <- sum(x^2)
  varX <- n*(varX/n - meanX^2)/(n-1)
  sdX <-  sqrt(varX)
  zdata <- scale(x)
  
  meanH3 <- sum((zdata^3 - 3.0*zdata)/sqrt(6)) / sqrt(n)
  meanH4 <- sum((zdata^4 - 6*zdata^2 + 3)/(2*sqrt(6))) / sqrt(n)
  meanH5 <- sum((zdata^5 - 10*zdata^3 + 15*zdata)/(2*sqrt(30))) / sqrt(n)
  meanH6 <- sum((zdata^6 - 15*zdata^4 + 45*zdata^2 - 15)/(12*sqrt(5))) / sqrt(n)
  meanH7 <- sum((zdata^7 - 21*zdata^5 + 105*zdata^3 - 105*zdata)/(12*sqrt(35))) / sqrt(n)
  meanH8 <- sum((zdata^8 - 28*zdata^6.0 + 210*zdata^4 - 420*zdata^2 + 105)/(24*sqrt(70))) / sqrt(n)

  vectoraux1 <- meanH4 + meanH5 * zdata / sqrt(2) + meanH6 * (zdata^2 - 1) /
    sqrt(6) + meanH7 * (zdata^3 - 3 * zdata)/ (2 * sqrt(6)) + meanH8 *
    (zdata^4 - 6 * zdata^2 + 3.0) / (2 * sqrt(30))
  
  statTS1 <- abs(pnorm(zdata[1])*meanH3 - dnorm(zdata[1])*vectoraux1[1])
  for (i in 2:n) {
    tmp <- abs(pnorm(zdata[i])*meanH3 - dnorm(zdata[i])*vectoraux1[i])
    if (statTS1 < tmp){
      statTS1 <- tmp
    } 
  }
  
  result <- list(method = "Cabana-Cabana(1994), #1",
                 doi = "https://doi.org/10.1214/aos/1176325636",
                 statistic = statTS1,
                 df = NA,
                 p.value = NA)

  return(result)
}
