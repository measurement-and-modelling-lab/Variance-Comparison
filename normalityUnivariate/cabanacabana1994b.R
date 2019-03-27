cabanacabana1994b <- function(x){
  ## The 2nd Cabana-Cabana(1994) test for normality
  ## https://doi.org/10.1214/aos/1176325636
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat20.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
  n <- length(x)
  varX <- meanX <- sdX <- H3tilde <- H4tilde <- H5tilde <- H6tilde <- H7tilde <- H8tilde <- 0
  z <- H0 <- H1 <- H2 <- H3 <- H4 <- H5 <- H6 <- H7 <- H8 <- matrix(0,n,0)
  vectoraux2 <- matrix(0,n,0)
  meanX <- mean(x)
  for(i in 1:n){
    varX <- varX + x[i]^2  
  }
  varX <- n*(varX/n - meanX^2)/(n-1)
  sdX <-  sqrt(varX)
  for(i in 1:n){
    z[i] <- (x[i] - meanX)/sdX
  }
  
  for (i in 1:n) {
    
    H0[i] <- 1
    H1[i] <- z[i]
    H2[i] <- (z[i]^2 - 1)/sqrt(2)
    H3[i] <- (z[i]^3 - 3*z[i])/sqrt(6)
    H4[i] <- (z[i]^4 - 6*z[i]^2 + 3)/(2*sqrt(6))
    H5[i] <- (z[i]^5 - 10*z[i]^3 + 15*z[i])/(2*sqrt(30))
    H6[i] <- (z[i]^6 - 15*z[i]^4 + 45*z[i]^2 - 15)/(12*sqrt(5))
    H7[i] <- (z[i]^7 - 21*z[i]^5 + 105*z[i]^3 - 105*z[i])/(12*sqrt(35))
    H8[i] <- (z[i]^8 - 28*z[i]^6 + 210*z[i]^4 - 420*z[i]^2 + 105)/(24*sqrt(70))
    
    H3tilde <- H3tilde + H3[i]   
    H4tilde <- H4tilde + H4[i]  
    H5tilde <- H5tilde + H5[i]  
    H6tilde <- H6tilde + H6[i]  
    H7tilde <- H7tilde + H7[i]  
    H8tilde <- H8tilde + H8[i]
    
  }
  
  H3tilde <- H3tilde/sqrt(n)
  H4tilde <- H4tilde/sqrt(n)
  H5tilde <- H5tilde/sqrt(n)
  H6tilde <- H6tilde/sqrt(n)
  H7tilde <- H7tilde/sqrt(n)
  H8tilde <- H8tilde/sqrt(n)
  
  
  for (i in 1:n) {
    vectoraux2[i] = (sqrt(2/1)*H0[i] + H2[i])*H5tilde + (sqrt(3/2)*H1[i] + H3[i])*H6tilde + (sqrt(4/3)*H2[i] + H4[i])*H7tilde + (sqrt(5/4)*H3[i] + H5[i])*H8tilde + (sqrt(5/4)*H3[i] + H5[i])*H8tilde
  }
  statTK1 <-  abs(-dnorm(z[1])*H3tilde +(pnorm(z[1])-z[1]*dnorm(z[1]))*H4tilde - dnorm(z[1])*vectoraux2[1])
  for (i in 2:n) {
    tmp <- abs(-dnorm(z[i])*H3tilde +(pnorm(z[i])-z[i]*dnorm(z[i]))*H4tilde - dnorm(z[i])*vectoraux2[i])
    if (statTK1 < tmp){
      statTK1 <- tmp
    } 
  }
  
  
  result <- list(method = "Cabana-Cabana(1994), #2",
                 doi = "https://doi.org/10.1214/aos/1176325636",
                 statistic = statTK1,
                 df = NA,
                 p.value = NA)
  return(result)
}