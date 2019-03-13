bontempsmeddahi2005a <- function(x){
## The 1st Bontemps-Meddahi(2005) test for normality
## https://doi.org/10.1016/j.jeconom.2004.02.014
## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat14.cpp
    
  n<-length(x)
  varX <- statBM34 <- tmp3 <- tmp4 <- 0 
  z<- matrix(0,n,0)
  meanX <- mean(x)
  for (i in 1:n){
    z[i] <- (x[i] - meanX)/sd(x)
  }
  for (i in 1:n){
    tmp3 <- tmp3 + (z[i]^3 - 3*z[i])/sqrt(6)
    tmp4 <- tmp4 + ((z[i]^4 - 6*z[i]^2)+3)/(2 * sqrt(6))
    
  }
  statBM34 <- (tmp3^2 + tmp4^2)/n
  PVAL <- 1 - pchisq(statBM34, 2)
  
  result <- list(method = "Bontemps-Meddahi (2005), #1",
                 statistic = statBM34,
                 df = 2,
                 p.value = PVAL)
  return(result)
  
}
