epps1983 <- function(x){
  ##A test of normality based on empirical characteristic function  
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat31.cpp
  
  dyn.load("./normalityUnivariate/epps1983.so")
  n <- length(x)
  term1 <- term2 <- S2 <- 0
  
  xbar <- mean(x)
  alpha <- 1
  
  S2 <- mean(scale(x, scale=FALSE)^2)
  term1 <- .C("epps1983",
              x = x,
              term1 = term1,
              n = as.integer(n),
              S2 = as.double(S2))$term1
  term1 <- term1/n^2
  
  term2 <- sum(exp((-0.5*(x-xbar)^2)/(S2*(1+alpha^2))))
  term2 <- 2*(1+alpha^(-2))^(-.5) * term2 / n
  
  statTEP <- term1 - term2 + (1+2*alpha^(-2))^(-0.5)
  statTEP = -log(n*abs(statTEP))
  
  result <- list(method = "Epps-Pulley (1983)",
                 doi = "https://doi.org/10.1093/biomet/70.3.723",
                 statistic = statTEP,
                 df = NA,
                 p.value = NA)
  return(result)
  
}