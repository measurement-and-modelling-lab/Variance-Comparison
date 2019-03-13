mardia1970_kurtosis <- function(X) {
  ## Mardia (1970)
  ## https://doi.org/10.1093/biomet/57.3.519

  X <- as.matrix(X)
  
  n <- nrow(X)
  p <- ncol(X)

  S <- cov.wt(X, method="ML")$cov

  col_means <- apply(X, 2, mean)
  difT <- sweep(X, 2, col_means, "-")

  D <- difT%*%solve(S)%*%t(difT)
  b2p <- sum(diag(D^2))/n
  MKT = (b2p-(p*(p+2)*(n-1)/(n+1)))/(sqrt((8*p*(p+2))/n))
  p = 2*(1-pnorm(abs(MKT), 0, 1))

  result <- list(method = "Mardia (1970) kurtosis test",
                 statistic = MKT,
                 df = NA,
                 p.value = p)

  return(result)
}
