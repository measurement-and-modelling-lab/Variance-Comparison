zhang1999b <- function(x){
  ## Omnibus test of normality using the Q statistic
  ## https://doi.org/10.1080/02664769922395
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat34.cpp
  
  n <- length(x)
  q1star <- q2star <- term <- 0
  u <- matrix(0,n,0)
  a <- matrix(0,n,0)
  b <- matrix(0,n,0)
  x <- sort(x)
  
  for(i in 2:(n+1)){
    u[i-1] <- qnorm(((i-1)-0.375)/(n+0.25))
  }
  
  for(i in 3:(n+1)){
    a[i-1] <- 1/((n-1)*(u[i-1]-u[1]))
    term <- term + a[i-1]
  }
  a[1] <- -term
  
  #n must be greater than 8 to prevent crashing 
  if(n>8){
    b[1] <- 1/((n-4)*(u[1]-u[5]))
    b[n] <- -b[1]
    b[2] <- 1/((n-4)*(u[2]-u[6]))
    b[n-1] <- -b[2]
    b[3] = 1/((n-4)*(u[3]-u[7]))
    b[n-2] <- -b[3]
    b[4] <- 1/((n-4)*(u[4]-u[8]))
    b[n-3] <- -b[4]
    
    for(i in 6:(n-3)){
      b[i-1] <- (1/(u[i-1]-u[i+3]) - 1/(u[i-5]-u[i-1]))/(n-4)
    }
    for(i in 2:(n+1)){
      q1star <- q1star - a[i-1]*x[n-(i-2)]
      q2star <- q2star - b[i-1]*x[n-(i-2)]
    }
    
    Qstar <- log(q1star/q2star)
    
    result <- list(method = "Zhang, P (1999), #2",
                   statistic = Qstar,
                   df = NA,
                   p.value = NA)
    
    return(result)
    
  }else{
    result <- list(method = "Zhang, P (1999), #2",
                   doi = "https://doi.org/10.1080/02664769922395",
                   statistic = NA,
                   df = NA,
                   p.value = NA)
    return(result)
  }
  
}