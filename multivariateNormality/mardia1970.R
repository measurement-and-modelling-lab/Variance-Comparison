mardia1970_skewness <- function(X) {
    ## Mardia's (1970) tests of multivariate skewness
    ## https://doi.org/10.1093/biomet/57.3.519

    X <- as.matrix(X)
    
    n <- nrow(X)
    p <- ncol(X)

    S <- cov.wt(X, method="ML")$cov

    col_means <- apply(X, 2, mean)
    difT <- sweep(X, 2, col_means, "-")

    D <- difT%*%solve(S)%*%t(difT)
    b1p <- sum(colSums(D^3))/n^2

    df = (p*(p+1)*(p+2))/6
    MST = (n*b1p)/6
    p = 1 - pchisq(MST,df)

    result <- list(method = "Mardia's (1970) skewness test",
                   statistic = MST,
                   df = NA,
                   p.value = p)

    return(result)
}

mardia1970_kurtosis <- function(X) {
    ## Mardia's (1970) tests of multivariate kurtosis
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

    result <- list(method = "Mardia's (1970) kurtosis test",
                   statistic = MKT,
                   df = NA,
                   p.value = p)

    return(result)
}
