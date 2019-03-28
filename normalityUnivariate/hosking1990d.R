hosking1990d <- function(x){
  ## The 4th Hosking test for normality(1990)
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat13.cpp

  pstarmod3 <- function(r,n,i){
    res <- 0
    for (k in 0:(r-1)) {
      res <- res + (-1)^k * choose(r-1,k) * choose(i-1,r+2-k) * choose(n-i,3+k)
      
    }
    
    return(res)
    
  }
  x <- sort(x)
  n <- length(x)
  l23<-l33<-l43<-0
  for(i in 3:n){
    l23 <- l23 + x[i-1]*pstarmod3(2,n,i-1)
    l33 <- l33 + x[i-1]*pstarmod3(3,n,i-1)
    l43 <- l43 + x[i-1]*pstarmod3(4,n,i-1)
    
  }
  l23 <- l23/(2*choose(n,8))
  l33 <- l33/(3*choose(n,9))
  l43 <- l43/(4*choose(n,10))
  
  tau33 <- l33/l23
  tau43 <- l43/l23
  
  if (1<=n & n<=25) {
    mutau43 <- 0.033180
    vtau33 <- 0.0095765
    vtau43 <- 0.0044609
  }
  
  if (25<n & n<=50) {
    mutau43 <- 0.028224
    vtau33 <- 0.0033813
    vtau43 <- 0.0011823
  }
  
  if (50<n) {
    mutau43 <- 0.026645
    vtau33 <- 0.0014547
    vtau43 <- 0.00045107
  }
  
  statTLmom3 <- tau33^2 / vtau33 + (tau43 - mutau43)^2 / vtau43
  PVAL <- 1 - pchisq(statTLmom3, 2)
  
  result <- list(method = "Hosking (1990), #4",
                 doi = "https://doi.org/10.1111/j.2517-6161.1990.tb01775.x",
                 statistic = statTLmom3,
                 df = 2,
                 p.value = PVAL)
  return(result)
  
}
