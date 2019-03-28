gelgastwirth2008 <- function(x){
  ## The Gel-Gastwirth2008 test for normality
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat9.cpp

  meanX <-  mean(x)
  n <- length(x)
  m3<-0
  m4<-0
  Jn <- 0
  for(i in 1:n){
    m3<- m3 + (x[i]-meanX)^3
    m4<- m4 + (x[i]-meanX)^4
  }
  m3<- m3/n
  m4 <- m4/n
  x <- sort(x)
  if (n%%2 == 0) {
    M<-(x[n/2+1]+x[n/2])/2
  }
  else{
    M<- x[n/2+1]
  }
  for (i in 1:n){
    Jn <- Jn + abs(x[i]-M)
  }
  Jn <- sqrt(pi/2) * Jn/n
  print(m3)
  stat <- n*((m3/(Jn^3))^2)/6+n*((m4/(Jn^4)-3)^2)/64
  PVAL <- 1 - pchisq(stat, 2)
  
  result <- list(method = "Gel and Gastwirth (2008)",
                 doi = "https://doi.org/10.1016/j.econlet.2007.05.022",
                 statistic = stat,
                 df = 2,
                 p.value = PVAL)
  return(result)
  
}
