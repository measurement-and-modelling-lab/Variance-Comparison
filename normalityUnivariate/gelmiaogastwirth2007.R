gelmiaogastwirth2007 <- function(x){
  ## Robust directed tests of normality against heavy-tailed alternative
  ## https://doi.org/10.1016/j.csda.2006.08.022
  ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat33.cpp
  ## bilateral test that rejects H0 only for large values of the test statistic
  
   n <- length(x)
   varX <- Jn <- 0
   meanX <- mean(x)
   M_PI <- 3.14159265358979323846264338327950288
   
   for (i in 1:n) {
     varX <- varX + (x[i]-meanX)^2
   }
   
   varX <- varX/n
   sdX <- sqrt(varX)
   x <- sort(x)
   
   if ((n%%2) == 0) {
     M <- (x[n/2+1]+x[n/2])/2
   } else {
     M <- x[n/2+1]
   }
   
   for (i in 1:n){
     Jn <- Jn + abs(x[i]-M)
   }
   
   Jn <- sqrt(M_PI/2)* (Jn/n)
   statRsJ <- sdX/Jn
   tmp <- pnorm(statRsJ, 1.0, sqrt((M_PI/ 2.0 - 1.5) /(n)))
   if (tmp > 0.5) {
     tmp <- 1.0 - tmp
   }
  result <- list(method = "Gel, Y.R., Miao, W. and Gastwirth, J.L. (2007)",
                 doi = "https://doi.org/10.1016/j.csda.2006.08.022",
                 statistic = statRsJ,
                 df = NA,
                 p.value = tmp)
  return(result)
  
}