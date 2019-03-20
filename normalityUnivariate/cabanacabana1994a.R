cabanacabana1994a <- function(x){
  ## The 1st Cabana-Cabana(1994) test for normality
  ## https://doi.org/10.1214/aos/1176325636
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat19.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
  n <- length(x)
  varX <- meanX <- sdX <- meanH3 <- meanH4 <- meanH5 <- meanH6 <- meanH7 <- meanH8 <- 0
  zdata <- matrix(0,n,0)
  vectoraux1 <- matrix(0,n,0)
  meanX <- mean(x)
  for(i in 1:n){
    varX <- varX + x[i]^2  
  }
  varX <- n*(varX/n - meanX^2)/(n-1)
  sdX <-  sqrt(varX)
   for(i in 1:n){
    zdata[i] <- (x[i] - meanX)/sdX
   }
  
  for (i in 1:n) {
    
      meanH3 <- meanH3 + (zdata[i]^3 - 3.0*zdata[i])/sqrt(6)   

      meanH4 <- meanH4 + (zdata[i]^4 - 6*zdata[i]^2 + 3)/(2*sqrt(6))  

      meanH5 <- meanH5 + (zdata[i]^5 - 10*zdata[i]^3 + 15*zdata[i])/(2*sqrt(30)) 

      meanH6 <- meanH6 + (zdata[i]^6 - 15*zdata[i]^4 + 45*zdata[i]^2 - 15)/(12*sqrt(5)) 

      meanH7 <- meanH7 + (zdata[i]^7 - 21*zdata[i]^5 + 105*zdata[i]^3 - 105*zdata[i])/(12*sqrt(35))  

      meanH8 <- meanH8 + (zdata[i]^8 - 28*zdata[i]^6.0 + 210*zdata[i]^4 - 420*zdata[i]^2 + 105)/(24*sqrt(70)) 
      
  }
  
    meanH3 <- meanH3/sqrt(n)
    meanH4 <- meanH4/sqrt(n)
    meanH5 <- meanH5/sqrt(n)
    meanH6 <- meanH6/sqrt(n)
    meanH7 <- meanH7/sqrt(n)
    meanH8 <- meanH8/sqrt(n)
    
    
    for (i in 1:n) {
      vectoraux1[i] <- meanH4 + meanH5*zdata[i]/sqrt(2) + meanH6*(zdata[i]^2 - 1)/sqrt(6) + meanH7*(zdata[i]^3 - 3*zdata[i])/(2*sqrt(6)) + meanH8*(zdata[i]^4 - 6*zdata[i]^2 + 3.0)/(2*sqrt(30))
    }
    statTS1 <- abs(pnorm(zdata[1])*meanH3 - dnorm(zdata[1])*vectoraux1[1])
    for (i in 2:n) {
      tmp <- abs(pnorm(zdata[i])*meanH3 - dnorm(zdata[i])*vectoraux1[i])
      if (statTS1 < tmp){
        statTS1 <- tmp
      } 
    }
  
    
  result <- list(method = "Cabana-Cabana(1994), #1",
                 statistic = statTS1,
                 df = NA,
                 p.value = NA)
  return(result)
}