filliben1975 <- function(x){
  ## Filliben, J.J. (1975), The Probability Plot Correlation Coefficient Test for Normality
  ## http://doi.org/10.1080/00401706.1975.10489279
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat25.cpp
  ## bilateral test that rejects H0 only for small values of the test statistic
  
  n <- length(x) 
  varX <- term1 <- term2 <-0
  M <- matrix(0,n,0)
  x<- sort(x) 
  meanX <- mean(x)
  for(i in 1:n){
    varX <- varX + x[i]^2
  }
  varX <- (varX - (n*(meanX^2)))/(n-1)
  
  M[1] <- qnorm(1-(0.5^(1/n)))
  M[n] <- qnorm(0.5^(1/n))
  
  for(i in 3:n){
    M[i-1] <- qnorm(((i-1)-0.3175)/(n+0.365))
  }
  for(i in 1:n){
    term1 <- term1 + x[i]*M[i]
    term2 <- term2 + M[i]*M[i]
  }
  statr <- term1 / (sqrt(term2*(n-1)*varX))
  
  result <- list(method = "Filliben, J.J. (1975)",
                 statistic = statr,
                 df = NA,
                 p.value = NA)
  return(result)
}