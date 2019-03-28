## The Bonett-Seier(2002) test for normality
## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat17.cpp

bonettseier2002 <- function(x){
  n <- length(x)
  m2 <- term <- 0
  meanX <- mean(x)
  
  for(i in 1:n){
    m2 <- m2 + (x[i]-meanX)^2
    term <- term + abs(x[i]-meanX)
  }
  m2 <- m2/n
  term <- term/n
  
  omega <- 13.29*(log(sqrt(m2))-log(term))
  statTw <- sqrt(n+2)*(omega-3)/3.54
  
  pval <- 2*pnorm(abs(statTw),lower.tail = FALSE)
  
  result <- list(method = "Bonett-Seier (2002)",
                 doi = "https://doi.org/10.1016/S0167-9473(02)00074-9",
                 statistic = statTw,
                 df = NA,
                 p.value = pval)
  return(result)
}
