source("./shapeMultivariate/wilcoxon1945a.R")
sandvik1982 <- function(x){
  X <- x[,1]
  Y <- x[,2]
  Mx <- median(X)
  My <- median(Y)
  Z<- abs(X-Mx) - abs(Y-My)
  Z <- Z[Z!=0]
  stat <- wilcoxon1945a(Z)
  result <- list(method = "Sandvik 1982",
                 doi = "https://doi.org/10.1093/biomet/69.2.484",
                 statistic = stat$statistic,
                 df = NA,
                 p.value = stat$p.value)
  return(result)
}

