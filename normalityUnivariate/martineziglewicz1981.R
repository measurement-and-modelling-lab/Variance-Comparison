martineziglewicz1981 <- function(x){
  ## A test for departure from normality based on a biweight estimator of scale
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat32.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
  n <- length(x)
  aux1 <-  matrix(0,n,0)
  z <-  matrix(0,n,0)
  term1 <- term2 <- term3 <- 0
  
  x <- sort(x)
  if(n%%2 == 0){
    M <- (x[n/2+1]+x[n/2])/2
  }else{
    M <- x[n/2+1]
  }
  for(i in 1:n){
    aux1[i] <- x[i] - M
    x[i] <- abs(aux1[i])
  }
  x <- sort(x) 
  if(n%%2 == 0){
    A <- (x[n/2+1]+x[n/2])/2
  }else{
    A <- x[n/2+1]
  }
  
  A <- 9 * A
  
  for(i in 1:n){
    z[i] <- aux1[i]/ A
    if(abs(z[i]) < 1){
      z2 <- z[i]^2
      term1 <- term1 + aux1[i]^2 * (1-z2)^4
      term2 <- term2 + (1-z2) * (1-5*z2)
    }
    term3 <- term3 + aux1[i]^2
  }
  
  Sb2 <- n*term1/term2^2
  
  statIn <- (term3/(n-1)) / Sb2
  
  
  result <- list(method = "Martinez-Iglewicz (1981)",
                 doi = "https://doi.org/10.1093/biomet/68.1.331",
                 statistic = statIn,
                 df = NA,
                 p.value = NA)
  return(result)
  
}
