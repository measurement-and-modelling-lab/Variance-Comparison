eppspulley1983 <- function(x){
  ##A test of normality based on empirical characteristic function  
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat31.cpp
  
  n <- length(x)
  term1 <- term2 <- S2 <- 0
  
  xbar <- mean(x)
  alpha <- 1
  
  for(i in 1:n){
    S2 <- S2 + (x[i]-xbar)^2
  }
  S2 <- S2/n
  
  for(i in 1:n ){
    for(j in 1:n){
      term1 <- term1 + exp((-0.5*(x[i]-x[j])^2)/(alpha^2*S2))
    }
  }
  term1 <- term1/n^2
  
  for(i in 1:n){
    term2 <- term2 + exp((-0.5*(x[i]-xbar)^2)/(S2*(1+alpha^2)))
  }
  term2 <- 2*(1+alpha^(-2))^(-.5) * term2 / n
  
  statTEP <- term1 - term2 + (1+2*alpha^(-2))^(-0.5)
  statTEP = -log(n*abs(statTEP))
  
  result <- list(method = "Epps, T.W. and Pulley, L.B. (1983)",
                 doi = "https://doi.org/10.1093/biomet/70.3.723",
                 statistic = statTEP,
                 df = NA,
                 p.value = NA)
  return(result)
  
}
