glen2001 <- function(x){
  ## The Glen-Leemis-Barr normality test
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat38.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
  plaplace <- function(y) {
    temp = 0.5 * exp(-abs(y))
    if (y<=0){
      return (temp)
    }else{
      return(1.0 - temp)
    }
  }
  

  n <- length(x)
  tmp <- statPS <- 0
  Phiz <- matrix(0,n,0)
  x<- sort(x)
  
  if (n %% 2 == 0) {
    muhat <- (x[n/2] + x[n/2+1]) / 2
  } else {
    muhat <- x[n/2+1]
  }
  
  tmp <- sum(abs(x - muhat))
  bhat <- tmp/n
  for (i in 1:n) {
    Phiz[i] <- plaplace((x[i] - muhat) / bhat)
  }
  Phiz <- sort(Phiz)
  
  for (i in 2:(n+1)) {
    Phiz[i - 1] <- pbeta(Phiz[i - 1], (i-1), (n - (i-1) + 1))
  }
  Phiz <- sort(Phiz)
  for (i in 2:(n+1)) {
    statPS <- statPS + (2 * n + 1 - 2 * (i-1)) * log(Phiz[i - 1]) + (2 * (i-1) - 1) * log(1 - Phiz[i - 1]);
  }
  stat <- -n - statPS/n
  result <- list(method = "Glen-Leemis-Barr (2001)",
                 doi = "https://www.doi.org/10.1109/24.963129",
                 statistic = stat,
                 df = NA,
                 p.value = NA)
  
  return(result)
  
  
  
}
