rahman1997 <- function(x){
  ## Rahman, M.M. and Govindarajulu, Z. (1997), A modification of the test of Shapiro and Wilk for normality
  ## https://doi.org/10.1080/02664769723828
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat23.cpp
  ## bilateral test that rejects H0 only for small values of the test statistic
  
  n <- length(x)
  xs <- mi <- fi <- aux1 <- aux2 <- aux3 <- aux4  <- aistar  <- ai <- matrix(0,n,0)
  norm2<- aux6 <- statWRG <-0
  for(i in 2:(n+1)){
    mi[i-1] <- qnorm((i-1)/(n+1))
    fi[i-1] <- dnorm(mi[i-1])
    aux2[i-1] <- 2*mi[i-1]*fi[i-1]
  }
  
  aux1[1] <- 0
  for (i in 2:n) { 
    aux1[i] <- mi[i-1]*fi[i-1]
  }
  
  for (i in 1:(n-1)) { 
    aux3[i] <- mi[i+1]*fi[i+1]
  }
  aux3[n] <- 0
  
  for (i in 1:n) { 
    aux4[i] <- aux1[i] - aux2[i] + aux3[i]
    aistar[i] <- -((n+1)*(n+2))*fi[i]*aux4[i]
    norm2 <- norm2 + aistar[i]^2
  }
  
  for (i in 1:n) { 
    ai[i] <- aistar[i]/sqrt(norm2)
  }
  x <- sort(x)
  meanX <- mean(x)
  
  for (i in 1:n) { 
    aux6 <- aux6 + (x[i]-meanX)^2
  }
  
  for (i in 1:n) { 
    statWRG <- statWRG + ai[i]*x[i]
  }
  statWRG <- statWRG^2/aux6
  
  
  result <- list(method = "Rahman, M.M. and Govindarajulu, Z. (1997)",
                 doi = "https://doi.org/10.1080/02664769723828",
                 statistic = statWRG,
                 df = NA,
                 p.value = NA)
  return(result)
}